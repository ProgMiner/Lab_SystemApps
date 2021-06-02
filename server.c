#include "server.h"

#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/socket.h>

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
    struct socket_handler_context * socket_context;
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
};

static const char * RESPONSE_DATA =
        "HTTP/1.1 200 OK\r\n"
        "Content-Length: 3\r\n"
        "Content-Type: text/plain; charset=utf-8\r\n"
        "\r\n"
        "KEKE";

static char * make_request_path(const char * work_dir, const char * path) {
    size_t work_dir_length, path_length, request_path_length;
    char * request_path, * real_request_path;

    if (!work_dir || !path) {
        return NULL;
    }

    work_dir_length = strlen(work_dir);
    path_length = strlen(path);

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

    printf("[%d] Wrote %d bytes. Remaining: %lu bytes\n", context->socket_context->socket, ret,
            buffer_remaining(context->shadow_buffer));

poll_thread_continue:
    ret = poll_thread_continue(context->socket_context->server_context->poll_thread,
            context->socket_handler_descriptor);

    if (ret < 0) {
        goto free_request;
    }

    goto end;

    /* TODO: think about free */

free_request:
    poll_thread_unregister(context->socket_context->server_context->poll_thread, event.descriptor);
    http_request_parser_delete(context->socket_context->request_parser);
    http_request_delete(context->socket_context->request);
    buffer_delete(context->socket_context->buffer);
    close(context->socket_context->socket);
    free(context->socket_context);
    buffer_delete(context->buffer);
    free(context->buffer);
    buffer_delete(context->shadow_buffer);
    free(context->shadow_buffer);
    free(context);

end:
    return ret;

static int http_request_handler_config_handler(
        struct request_context * context,
        struct config config
) {
    printf("[%d] Config got!\n", context->socket);
    return 0;
}

/* TODO send error codes */
static int http_request_handler(
        struct socket_handler_context * context,
        int poll_thread_descriptor
) {
    char * request_path;
    int ret = 0, socket_write_handler_descriptor;
    struct socket_write_handler_context * socket_write_handler_context;
    struct request_context * request_context;

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

    if (!request_context->request_path) {
        ret = -errno;
        goto free_request_context;
    }

    printf("[%d] Request path: %s\n", context->socket, request_context->request_path);

    request_context->server_context = context->server_context;
    request_context->request = context->request;
    request_context->socket = context->socket;

    request_context->socket_handler_context = context;

    socket_write_handler_context = malloc(sizeof(struct socket_write_handler_context));
    if (!socket_write_handler_context) {
        ret = -ENOMEM;
        goto free_request_path;
    }

    socket_write_handler_context->socket_context = context;
    socket_write_handler_context->socket_handler_descriptor = poll_thread_descriptor;

    socket_write_handler_context->buffer = buffer_new(4096);
    if (!socket_write_handler_context->buffer) {
        ret = -errno;
        goto free_socket_write_handler_context;
    }

    socket_write_handler_context->shadow_buffer = buffer_shadow(socket_write_handler_context->buffer);
    if (!socket_write_handler_context->shadow_buffer) {
        ret = -errno;
        goto free_socket_write_handler_context;
    }

    buffer_write(socket_write_handler_context->buffer, (uint8_t *) RESPONSE_DATA, strlen(RESPONSE_DATA));
    buffer_write_shadow(socket_write_handler_context->buffer, socket_write_handler_context->shadow_buffer);

    socket_write_handler_descriptor = poll_thread_register(context->server_context->poll_thread, context->socket, POLLOUT,
            poll_thread_handler(socket_write_handler_context, socket_write_handler));
    if (socket_write_handler_descriptor < 0) {
        ret = socket_write_handler_descriptor;
        goto free_socket_write_handler_context;
    }

    /* TODO: think about several requests sequential! (pipelining) */

    goto end;

free_socket_write_handler_context:
    free(socket_write_handler_context);

    ret = config_manager_resolve(
            context->server_context->config_manager,
            request_context->request_path,
            config_manager_handler(http_request_handler_config_handler, request_context)
    );
    if (ret) {
        goto free_request_path;
    }

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
        return ret;
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

    ret = poll_thread_run(poll_thread);

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
