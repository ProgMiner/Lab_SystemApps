#include "server.h"

#include <time.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/wait.h>

#include "poll_thread.h"
#include "buffer.h"
#include "config.h"
#include "http.h"
#include "util.h"


struct server_context {
    char * work_dir;

    struct poll_thread * poll_thread;
    struct config_manager * config_manager;
};

struct server_socket_handler_context {
    struct server_context * server_context;
    int server_socket;
};

struct socket_handler_context {
    struct server_context * server_context;
    struct http_request_parser * request_parser;
    struct http_request * request;
    struct buffer * buffer;
    int socket;
};

struct socket_write_handler_context {
    struct socket_handler_context * socket_handler_context;
    int socket_handler_descriptor;
    struct buffer * buffer;
    struct buffer * shadow_buffer;
};

struct request_context {
    struct server_context * server_context;
    struct http_request * request;
    char * request_path;
    int socket;

    struct socket_handler_context * socket_handler_context;
    int socket_handler_descriptor;
};

static const char * not_found_response =
        "HTTP/1.1 404 Not Found\r\n"
        "Content-Length: 111\r\n"
        "Content-Type: text/html; charset=utf-8\r\n"
        "\r\n"
        "<html>\n"
        "<head>\n"
        "    <title>404 - Not Found</title>\n"
        "</head>\n"
        "<body>\n"
        "    <h1>File not found :(</h1>\n"
        "</body>\n"
        "</html>\n";

static const char * forbidden_response =
        "HTTP/1.1 403 Forbidden\r\n"
        "Content-Length: 114\r\n"
        "Content-Type: text/html; charset=utf-8\r\n"
        "\r\n"
        "<html>\n"
        "<head>\n"
        "    <title>403 - Forbidden</title>\n"
        "</head>\n"
        "<body>\n"
        "    <h1>Access forbidden >:(</h1>\n"
        "</body>\n"
        "</html>\n";

static const char * http_mime_header =
        "HTTP/1.1 200 OK\r\n"
        "Content-Length: %d\r\n"
        "Content-Type: %s; charset=utf-8\r\n"
        "\r\n";

static char * make_request_path(const char * work_dir, const char * path) {
    size_t work_dir_length, path_length, request_path_length;
    char * path_file, * request_path, * real_request_path;

    if (!work_dir || !path) {
        return NULL;
    }

    path_file = strnextract(path, strlen(path), '?');
    if (!path_file) {
        path_file = strdup(path);

        if (!path_file) {
            return NULL;
        }
    }

    work_dir_length = strlen(work_dir);
    path_length = strlen(path_file);
    free(path_file);

    request_path_length = work_dir_length + path_length;
    request_path = malloc(sizeof(char) * (request_path_length + 1));
    if (!request_path) {
        return NULL;
    }

    strncpy(request_path, work_dir, request_path_length);
    strncat(request_path, path, path_length);

    real_request_path = realpath(request_path, NULL);
    if (!real_request_path) {
        free(request_path);
        return NULL;
    }

    free(request_path);

    if (!strstartswith(real_request_path, work_dir)) {
        free(real_request_path);
        errno = EACCES;
        return NULL;
    }

    if (real_request_path[work_dir_length] != '\0' && real_request_path[work_dir_length] != '/') {
        free(real_request_path);
        errno = EACCES;
        return NULL;
    }

    return real_request_path;
}

static int socket_write_handler(
        struct socket_write_handler_context * context,
        struct poll_thread_event event
) {
    int ret = 0;

    if (buffer_remaining(context->shadow_buffer) == 0) {
        goto poll_thread_continue;
    }

    ret = buffer_write_fd(context->shadow_buffer, event.fd);
    if (ret <= 0) {
        goto free_request;
    }

    printf("[%d] Wrote %d bytes. Remaining: %lu bytes\n", context->socket_handler_context->socket,
            ret, buffer_remaining(context->shadow_buffer));

poll_thread_continue:
    ret = poll_thread_continue(context->socket_handler_context->server_context->poll_thread,
            context->socket_handler_descriptor);

    if (ret < 0) {
        goto free_request;
    }

    goto end;

    /* TODO: think about free */

free_request:
    poll_thread_unregister(context->socket_handler_context->server_context->poll_thread, event.descriptor);
    http_request_parser_delete(context->socket_handler_context->request_parser);
    http_request_delete(context->socket_handler_context->request);
    buffer_delete(context->socket_handler_context->buffer);
    close(context->socket_handler_context->socket);
    free(context->socket_handler_context);
    buffer_delete(context->buffer);
    free(context->buffer);
    buffer_delete(context->shadow_buffer);
    free(context->shadow_buffer);
    free(context);

end:
    return ret;
}

static int http_request_send_response(
        struct socket_handler_context * socket_handler_context,
        int socket_handler_descriptor,
        uint8_t * bytes,
        size_t bytes_length
) {
    struct socket_write_handler_context * socket_write_handler_context;
    int ret = 0, socket_write_handler_descriptor;

    socket_write_handler_context = malloc(sizeof(struct socket_write_handler_context));
    if (!socket_write_handler_context) {
        ret = -ENOMEM;
        goto free_socket_write_handler_context;
    }

    socket_write_handler_context->socket_handler_context = socket_handler_context;
    socket_write_handler_context->socket_handler_descriptor = socket_handler_descriptor;

    socket_write_handler_context->buffer = buffer_new(bytes_length);
    if (!socket_write_handler_context->buffer) {
        ret = -errno;
        goto free_socket_write_handler_context;
    }

    socket_write_handler_context->shadow_buffer =
            buffer_shadow(socket_write_handler_context->buffer);
    if (!socket_write_handler_context->shadow_buffer) {
        ret = -errno;
        goto free_socket_write_handler_context;
    }

    buffer_write(socket_write_handler_context->buffer, bytes, bytes_length);
    buffer_write_shadow(socket_write_handler_context->buffer,
            socket_write_handler_context->shadow_buffer);

    socket_write_handler_descriptor = poll_thread_register(
            socket_handler_context->server_context->poll_thread,
            socket_handler_context->socket,
            POLLOUT,
            poll_thread_handler(socket_write_handler_context, socket_write_handler)
    );

    if (socket_write_handler_descriptor < 0) {
        ret = socket_write_handler_descriptor;
        goto free_socket_write_handler_context;
    }

    goto end;

free_socket_write_handler_context:
    free(socket_write_handler_context);

end:
    return ret;
}

static int cgi_exec(
        struct request_context * request_context,
        const char * script_path,
        const char * cgi_path
) {
    size_t body_length, body_written = 0, content_length;
    int child_in[2], child_out[2];
    const uint8_t * body;
    char * buf, * resp;
    const char * cbuf;
    char * envp[18];
    char * argv[3];
    unsigned int i;
    int ret = 0;
    pid_t cpid;

    argv[0] = strdup(cgi_path);
    argv[1] = strdup(script_path);
    argv[2] = NULL;

    if (!argv[0]) {
        return -errno;
    }

    if (pipe(child_in)) {
        free(argv[0]);
        return -errno;
    }

    if (pipe(child_out)) {
        free(argv[0]);
        close(child_in[0]);
        close(child_in[1]);
        return -errno;
    }

    cpid = fork();
    if (cpid < 0) {
        free(argv[0]);
        close(child_in[0]);
        close(child_in[1]);
        close(child_out[0]);
        close(child_out[1]);
        return -errno;
    }

    if (cpid == 0) {
        if (dup2(child_in[0], 0) < 0 || dup2(child_out[1], 1) < 0) {
            exit(-errno);
        }

        close(child_in[0]);
        close(child_in[1]);
        close(child_out[0]);
        close(child_out[1]);

        close(3); /* server socket */
        close(request_context->socket);

        /* TODO close opened files */
        /* That's funny security vulnerability =) */

        for (i = 0; i < 17; ++i) {
            envp[i] = malloc(sizeof(char) * 256);

            if (!envp[i]) {
                exit(-errno);
            }
        }

#define CGI_POPULATE_ENVP(_header, _var) \
        cbuf = http_headers_get(http_request_headers(request_context->request), _header); \
        if (cbuf) { \
            snprintf(envp[i++], 256, "%s=%s", _var, cbuf); \
        }

        i = 0;
        CGI_POPULATE_ENVP("Content-Length", "CONTENT_LENGTH");
        CGI_POPULATE_ENVP("Content-Type", "CONTENT_TYPE");
        strcpy(envp[i++], "GATEWAY_INTERFACE=CGI/1.1");
        strcpy(envp[i++], "SERVER_PROTOCOL=HTTP/1.1");
        strcpy(envp[i++], "REDIRECT_STATUS=404");
        snprintf(envp[i++], 256, "REQUEST_METHOD=%s", http_request_method(request_context->request));
        snprintf(envp[i++], 256, "SCRIPT_FILENAME=%s", script_path);
        snprintf(envp[i++], 256, "SCRIPT_NAME=%s", script_path);

        buf = strchr(http_request_path(request_context->request), '?');
        if (buf) {
            snprintf(envp[i++], 256, "QUERY_STRING=%s", buf + 1);
        }

        snprintf(envp[i], 256, "PATH_INFO=%s", http_request_path(request_context->request));
        if (buf) {
            envp[i][buf - http_request_path(request_context->request) + 10] = '\0';
        }

        ++i;

        for (; i < 17; ++i) {
            free(envp[i]);
            envp[i] = NULL;
        }

        /*
        "PATH_TRANSLATED"
        "REMOTE_ADDR"
        "REMOTE_HOST"
        "REMOTE_IDENT"
        "REMOTE_USER"
        "SERVER_NAME"
        "SERVER_PORT"
        "SERVER_SOFTWARE"
        */

        exit(-execve(cgi_path, argv, envp));
    }

    close(child_in[0]);
    close(child_out[1]);

    /* stdin of cgi is child_in[1] */
    /* stdout of cgi is child_out[0] */

    body = http_request_body(request_context->request, &body_length);
    while ((body_written = write(child_in[1], body, body_length)) > 0) {
        body_length += body_written;
        body += body_written;
    }

    if (body_written < 0) {
        ret = -errno;
        goto end;
    }

    cpid = waitpid(cpid, NULL, 0);
    if (cpid < 0) {
        ret = -errno;
        goto end;
    }

    buf = malloc(sizeof(char) * 1024);
    if (!buf) {
        ret = -ENOMEM;
        goto end;
    }

    resp = NULL;
    body_length = 0;

    while ((body_written = read(child_out[0], buf, 1024)) > 0) {
        resp = realloc(resp, sizeof(char) * (body_length + body_written));

        if (!resp) {
            ret = -errno;
            goto end;
        }

        memcpy(resp + body_length, buf, body_written);
        body_length += body_written;
    }

    printf("[%d] [INFO] Got %lu bytes from CGI!\n", request_context->socket, body_length);

    free(buf);
    if (body_written < 0) {
        free(resp);
        ret = -errno;
        goto end;
    }

    buf = strstr(resp, "Status:");
    if (buf) {
        buf = strtrim(strnsextract(buf + 7, body_length - (buf - resp) - 7, "\r\n"));

        if (buf) {
            printf("[%d] [INFO] CGI Status retrieved: %s\n", request_context->socket, buf);
        }
    } else {
        buf = strdup("200 OK");

        if (!buf) {
            free(resp);
            goto end;
        }
    }

    body_written = strlen(buf) + 11;
    resp = realloc(resp, sizeof(char) * (body_length + body_written));
    memmove(resp + body_written, resp, body_length);
    snprintf(resp, body_written, "HTTP/1.1 %s\r", buf);
    resp[body_written - 1] = '\n';
    body_length += body_written;
    free(buf);

    buf = strstr(resp + body_written, "\r\n\r\n");
    if (buf) {
        content_length = body_length - (buf - resp) - 4;
    } else {
        resp = realloc(resp, sizeof(char) * (body_length + 2));
        memmove(resp + body_written + 2, resp + body_written, body_length - body_written);
        resp[body_written] = '\r';
        resp[body_written + 1] = '\n';
        content_length = body_length - body_written;
        body_length += 2;
    }

    buf = malloc(sizeof(char) * 256);
    snprintf(buf, 256, "Content-Length: %lu\r\n", content_length);
    content_length = strlen(buf);

    resp = realloc(resp, sizeof(char) * (body_length + content_length));
    memmove(resp + body_written + content_length, resp + body_written, body_length - body_written);
    memcpy(resp + body_written, buf, content_length);
    body_length += content_length;
    free(buf);

    ret = http_request_send_response(request_context->socket_handler_context,
            request_context->socket_handler_descriptor, (uint8_t *) resp, body_length);

end:
    close(child_in[1]);
    close(child_out[0]);
    return ret;
}

static int http_request_send_file_mime(struct request_context * context, const char * mime_type) {
    uint8_t * response, * content;
    struct stat file_stat;
    int ret = 0, file_fd;
    size_t header_length;

    file_fd = open(context->request_path, 0, O_RDONLY);
    if (file_fd < 0) {
        ret = -errno;
        goto end;
    }

    ret = fstat(file_fd, &file_stat);
    if (ret) {
        ret = -errno;
        goto free_file_fd;
    }

    if (!S_ISREG(file_stat.st_mode)) {
        ret = http_request_send_response(context->socket_handler_context,
                context->socket_handler_descriptor, (uint8_t *) forbidden_response,
                strlen(forbidden_response));

        goto free_file_fd;
    }

    content = mmap(NULL, file_stat.st_size, PROT_READ, MAP_PRIVATE, file_fd, 0);
    if (content == MAP_FAILED) {
        ret = -errno;
        goto free_file_fd;
    }

    response = malloc(sizeof(uint8_t) * (strlen(http_mime_header) + strlen(mime_type)
            + file_stat.st_size + 256));

    if (!response) {
        ret = -ENOMEM;
        goto free_content;
    }

    sprintf((char *) response, http_mime_header, file_stat.st_size, mime_type);
    header_length = strlen((char *) response);

    memcpy(response + header_length, content, file_stat.st_size);
    ret = munmap(content, file_stat.st_size);
    if (ret) {
        ret = -errno;
        close(file_fd);
        goto free_response;
    }

    ret = close(file_fd);
    if (ret) {
        ret = -errno;
        goto free_response;
    }

    ret = http_request_send_response(context->socket_handler_context,
            context->socket_handler_descriptor, response, header_length + file_stat.st_size);
    if (ret) {
        goto free_response;
    }

    free(response);
    goto end;

free_response:
    free(response);
    goto end;

free_content:
    munmap(content, file_stat.st_size);

free_file_fd:
    close(file_fd);

end:
    return ret;
}

static int http_request_handler_config_handler(
        struct request_context * context,
        struct config config
) {
    int ret = 0;

    printf("[%d] Config has got!\n", context->socket);

    switch (config.action) {
    case CONFIG_ACTION_MIME:
        ret = http_request_send_file_mime(context, config.value.mime_type);

        if (ret) {
            goto free_context;
        }

        break;

    case CONFIG_ACTION_CGI:
        ret = cgi_exec(context, context->request_path, config.value.interpreter);

        if (ret) {
            goto free_context;
        }

        break;

    case CONFIG_ACTION_DENY:
        ret = http_request_send_response(context->socket_handler_context,
                context->socket_handler_descriptor, (uint8_t *) forbidden_response,
                strlen(forbidden_response));

        if (ret) {
            goto free_context;
        }

        break;
    }

    goto end;

free_context:
    free(context->request_path);
    close(context->socket);
    free(context);

end:
    return ret;
}

static int http_request_handler(
        struct socket_handler_context * context,
        int poll_thread_descriptor
) {
    struct request_context * request_context;
    struct stat file_stat;
    char * new_path;
    int ret = 0;

    printf("[%d] Request received\n", context->socket);

    request_context = malloc(sizeof(struct request_context));
    if (!request_context) {
        ret = -ENOMEM;
        goto end;
    }

    request_context->request_path = make_request_path(
            context->server_context->work_dir,
            http_request_path(context->request)
    );

    do {
        if (!request_context->request_path) {
            switch (errno) {
            case ENOENT:
                ret = http_request_send_response(context, poll_thread_descriptor,
                        (uint8_t *) not_found_response, strlen(not_found_response));

                break;

            case EACCES:
                ret = http_request_send_response(context, poll_thread_descriptor,
                        (uint8_t *) forbidden_response, strlen(forbidden_response));

                break;

            default:
                ret = -errno;
                break;
            }

            goto free_request_context;
        }

        printf("[%d] Request path: %s\n", context->socket, request_context->request_path);

        ret = stat(request_context->request_path, &file_stat);
        if (ret) {
            ret = -errno;
            goto free_request_path;
        }

        if (S_ISDIR(file_stat.st_mode)) {
            new_path = realloc(request_context->request_path, sizeof(char)
                    * (strlen(request_context->request_path) + 12));

            if (!new_path) {
                ret = -ENOMEM;
                goto free_request_path;
            }

            strcpy(new_path + strlen(new_path), "/index.html");
            request_context->request_path = new_path;

            ret = stat(request_context->request_path, &file_stat);
            if (ret) {
                free(request_context->request_path);
                request_context->request_path = NULL;
                ret = 0;
            }

            continue;
        }

        break;
    } while (true);

    request_context->server_context = context->server_context;
    request_context->request = context->request;
    request_context->socket = context->socket;

    request_context->socket_handler_context = context;
    request_context->socket_handler_descriptor = poll_thread_descriptor;

    ret = config_manager_resolve(
            context->server_context->config_manager,
            request_context->request_path,
            config_manager_handler(http_request_handler_config_handler, request_context)
    );
    if (ret) {
        goto free_request_path;
    }

    goto end;

free_request_path:
    free(request_context->request_path);

free_request_context:
    free(request_context);

end:
    return ret;
}

static int socket_handler(
        struct socket_handler_context * context,
        struct poll_thread_event event
) {
    enum http_request_parser_result parser_result;
    int ret;

    ret = buffer_read_fd(context->buffer, event.fd);
    if (ret < 0) {
        goto free_poll_thread_register;
    } else if (ret == 0) {
        ret = poll_thread_unregister(context->server_context->poll_thread, event.descriptor);
        goto free_context;
    }

    ret = 0;

    buffer_flip(context->buffer);
    parser_result = http_request_parser_parse(context->request_parser, context->buffer);
    switch (parser_result) {
    case HTTP_REQUEST_PARSER_RESULT_DONE:
        buffer_drop_start(context->buffer);

        http_request_parser_reset(context->request_parser);
        ret = http_request_handler(context, event.descriptor);
        if (ret) {
            goto free_poll_thread_register;
        }

        break;

    case HTTP_REQUEST_PARSER_RESULT_MORE:
        buffer_drop_start(context->buffer);

        ret = poll_thread_continue(context->server_context->poll_thread, event.descriptor);
        if (ret) {
            goto free_poll_thread_register;
        }

        break;

    case HTTP_REQUEST_PARSER_RESULT_INVAL:
        ret = poll_thread_unregister(context->server_context->poll_thread, event.descriptor);
        goto free_context;

    case HTTP_REQUEST_PARSER_RESULT_ERROR:
        ret = -errno;
        goto free_poll_thread_register;
    }

    goto end;

free_poll_thread_register:
    poll_thread_unregister(context->server_context->poll_thread, event.descriptor);

free_context:
    http_request_parser_delete(context->request_parser);
    http_request_delete(context->request);
    buffer_delete(context->buffer);
    close(context->socket);
    free(context);

end:
    if (ret) {
        errno = -ret;
        fprintf(stderr, "[%d] [WARN] ", context->socket);
        perror("Error while processing request");
        ret = 0;
    }

    return ret;
}

/* TODO handle connection closing */
static int handle_client_socket(
        struct server_socket_handler_context context,
        int socket,
        struct sockaddr_in6 addr
) {
    struct socket_handler_context * socket_handler_context;
    int socket_flags, ret = 0, socket_handler_descriptor;

    socket_flags = fcntl(socket, F_GETFL, 0);
    if (socket_flags < 0) {
        ret = socket_flags;
        goto end;
    }

    if ((ret = fcntl(socket, F_SETFL, socket_flags | O_NONBLOCK))) {
        goto end;
    }

    socket_handler_context = malloc(sizeof(struct socket_handler_context));
    if (!socket_handler_context) {
        ret = -ENOMEM;
        goto end;
    }

    socket_handler_context->buffer = buffer_new(1024);
    if (!socket_handler_context->buffer) {
        ret = -errno;
        goto free_socket_handler_context;
    }

    socket_handler_context->request = http_request_new();
    if (!socket_handler_context->request) {
        ret = -errno;
        goto free_buffer;
    }

    socket_handler_context->request_parser =
            http_request_parser_new(socket_handler_context->request);
    if (!socket_handler_context->request_parser) {
        ret = -errno;
        goto free_request;
    }

    socket_handler_context->server_context = context.server_context;
    socket_handler_context->socket = socket;

    socket_handler_descriptor = poll_thread_register(context.server_context->poll_thread, socket, POLLIN,
            poll_thread_handler(socket_handler_context, socket_handler));
    if (socket_handler_descriptor < 0) {
        ret = socket_handler_descriptor;
        goto free_request_parser;
    }

    printf("[%d] [INFO] Connection from: %02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x\n",
            socket,
            addr.sin6_addr.s6_addr[0], addr.sin6_addr.s6_addr[1],
            addr.sin6_addr.s6_addr[2], addr.sin6_addr.s6_addr[3],
            addr.sin6_addr.s6_addr[4], addr.sin6_addr.s6_addr[5],
            addr.sin6_addr.s6_addr[6], addr.sin6_addr.s6_addr[7],
            addr.sin6_addr.s6_addr[8], addr.sin6_addr.s6_addr[9],
            addr.sin6_addr.s6_addr[10], addr.sin6_addr.s6_addr[11],
            addr.sin6_addr.s6_addr[12], addr.sin6_addr.s6_addr[13],
            addr.sin6_addr.s6_addr[14], addr.sin6_addr.s6_addr[15]);

    goto end;

free_request_parser:
    http_request_parser_delete(socket_handler_context->request_parser);

free_request:
    http_request_delete(socket_handler_context->request);

free_buffer:
    buffer_delete(socket_handler_context->buffer);

free_socket_handler_context:
    free(socket_handler_context);

end:
    return ret;
}

static int server_socket_handler(
        struct server_socket_handler_context * context,
        struct poll_thread_event event
) {
    struct sockaddr_in6 addr;
    socklen_t addrlen = sizeof(addr);
    int socket, ret = 0;

    socket = accept(context->server_socket, (struct sockaddr *) &addr, &addrlen);
    if (socket < 0) {
        ret = -errno;
        goto free_context;
    }

    if (sizeof(addr) != addrlen) {
        ret = -EINVAL;
        goto free_context;
    }

    ret = poll_thread_continue(context->server_context->poll_thread, event.descriptor);
    if (ret) {
        goto free_socket;
    }

    ret = handle_client_socket(*context, socket, addr);
    if (ret) {
        goto free_socket;
    }

    goto end;

free_socket:
    close(socket);

free_context:
    free(context);

end:
    return ret;
}

static int server_stop_handler(struct poll_thread * poll_thread, struct poll_thread_event event) {
    static time_t prev_time = 0;
    int c;

    while ((c = getc(stdin)) != EOF) {
        switch (c) {
        case 'q':
            poll_thread_stop(poll_thread);
            return 0;

        case 'p':
            if (prev_time == 0) {
                prev_time = time(NULL);
                printf("Ping! %ld\n", prev_time);
            } else {
                printf("Pong! %ld sec\n", time(NULL) - prev_time);
                prev_time = 0;
            }

            break;
        }
    }

    return 0;
}

static void setup_sigaction_handler(int sig, siginfo_t * info, void * context) {
    static const char * msg = "Please, use 'q' instead of Ctrl+C.\n";

    write(STDOUT_FILENO, msg, strlen(msg));
}

static void setup_sigaction() {
    struct sigaction sa;

    sa.sa_sigaction = setup_sigaction_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO;

    sigaction(SIGINT, &sa, NULL);
}

int server_main(struct server_config config) {
    struct server_socket_handler_context * server_socket_handler_context;
    int server_socket, ret = 0, server_socket_handler_descriptor;
    struct server_context * server_context;
    struct poll_thread * poll_thread;
    tpool_t * thread_pool;

    struct sockaddr_in6 addr = { AF_INET6 };
    addr.sin6_port = htons(config.server_port);
    addr.sin6_addr = config.server_addr;

    server_socket = socket(AF_INET6, SOCK_STREAM | SOCK_NONBLOCK, /* TCP */ 6);
    if (server_socket < 0) {
        ret = -errno;
        goto end;
    }

    ret = bind(server_socket, (struct sockaddr *) &addr, sizeof(addr));
    if (ret < 0) {
        goto free_server_socket;
    }

    ret = listen(server_socket, 255);
    if (ret < 0) {
        goto free_server_socket;
    }

    thread_pool = tpool_create(8);

    poll_thread = poll_thread_new(thread_pool);
    if (!poll_thread) {
        ret = -errno;
        goto free_thread_pool;
    }

    server_context = malloc(sizeof(struct server_context));
    if (!server_context) {
        ret = -ENOMEM;
        goto free_poll_thread;
    }

    server_context->work_dir = realpath(config.work_dir, NULL);
    if (!server_context->work_dir) {
        ret = -errno;
        goto free_server_context;
    }

    server_context->config_manager = config_manager_new(poll_thread, server_context->work_dir);
    if (!server_context->config_manager) {
        ret = -errno;
        goto free_server_context_workdir;
    }

    server_context->poll_thread = poll_thread;

    server_socket_handler_context = malloc(sizeof(struct server_socket_handler_context));
    if (!server_socket_handler_context) {
        ret = -ENOMEM;
        goto free_server_context_config_manager;
    }

    server_socket_handler_context->server_context = server_context;
    server_socket_handler_context->server_socket = server_socket;

    server_socket_handler_descriptor = poll_thread_register(poll_thread, server_socket, POLLIN,
            poll_thread_handler(server_socket_handler_context, server_socket_handler));
    if (server_socket_handler_descriptor < 0) {
        ret = server_socket_handler_descriptor;
        goto free_server_socket_handler_context;
    }

    ret = poll_thread_register(poll_thread, STDIN_FILENO, POLLIN,
            poll_thread_handler(poll_thread, server_stop_handler));
    if (ret < 0) {
        goto free_server_socket_handler_descriptor;
    }

    setup_sigaction();

    ret = poll_thread_run(poll_thread);

    printf("Bye!\n");

free_server_socket_handler_descriptor:
    poll_thread_unregister(poll_thread, server_socket_handler_descriptor);

free_server_socket_handler_context:
    free(server_socket_handler_context);

free_server_context_config_manager:
    config_manager_delete(server_context->config_manager);

free_server_context_workdir:
    free(server_context->work_dir);

free_server_context:
    free(server_context);

free_poll_thread:
    poll_thread_delete(poll_thread);

free_thread_pool:
    tpool_destroy(thread_pool);

free_server_socket:
    close(server_socket);

end:
    return ret;
}
