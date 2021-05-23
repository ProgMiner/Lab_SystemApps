#include "server.h"

#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "poll_thread.h"
#include "buffer.h"
#include "http.h"


struct server_socket_handler_context {
    struct poll_thread * poll_thread;
    int server_socket;
};

struct socket_handler_context {
    struct poll_thread * poll_thread;
    struct http_request_parser * request_parser;
    struct http_request * request;
    struct buffer * buffer;
    int socket;
};

static int socket_handler(
        struct socket_handler_context * context,
        struct poll_thread_event event
) {
    enum http_request_parser_result parser_result;
    int ret;

    ret = buffer_read_fd(context->buffer, event.fd);
    if (ret < 0) {
        return ret;
    }

    ret = 0;

    buffer_flip(context->buffer);
    parser_result = http_request_parser_parse(context->request_parser, context->buffer);
    switch (parser_result) {
    case HTTP_REQUEST_PARSER_RESULT_DONE:
        buffer_drop_start(context->buffer);

        /* TODO */
        printf("[%d] Request received\n%s", event.fd, http_request_body(context->request, NULL));
        break;

    case HTTP_REQUEST_PARSER_RESULT_MORE:
        buffer_drop_start(context->buffer);
        ret = poll_thread_continue(context->poll_thread, event.descriptor);

        if (ret) {
            goto free_poll_thread_register;
        }

        break;

    case HTTP_REQUEST_PARSER_RESULT_INVAL:
        ret = poll_thread_unregister(context->poll_thread, event.descriptor);
        goto free_context;

    case HTTP_REQUEST_PARSER_RESULT_ERROR:
        ret = -errno;
        goto free_poll_thread_register;
    }

    goto end;

free_poll_thread_register:
    poll_thread_unregister(context->poll_thread, event.descriptor);

free_context:
    http_request_parser_delete(context->request_parser);
    http_request_delete(context->request);
    buffer_delete(context->buffer);
    close(context->socket);
    free(context);

end:
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

    socket_handler_context->poll_thread = context.poll_thread;
    socket_handler_context->socket = socket;

    socket_handler_descriptor = poll_thread_register(context.poll_thread, socket, POLLIN,
            poll_thread_handler(socket_handler_context, socket_handler));
    if (socket_handler_descriptor < 0) {
        ret = socket_handler_descriptor;
        goto free_request_parser;
    }

    printf("[%d] Connection from: %02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x:%02x%02x\n",
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

    ret = poll_thread_continue(context->poll_thread, event.descriptor);
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

    server_socket_handler_context = malloc(sizeof(struct server_socket_handler_context));
    if (!server_socket_handler_context) {
        ret = -ENOMEM;
        goto free_poll_thread;
    }

    server_socket_handler_context->poll_thread = poll_thread;
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

free_poll_thread:
    poll_thread_delete(poll_thread);

free_thread_pool:
    tpool_destroy(thread_pool);

free_server_socket:
    close(server_socket);

end:
    return ret;
}
