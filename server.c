#include "server.h"

#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "poll_thread.h"


struct server_socket_stream_handler_context {
    struct poll_thread * poll_thread;
    tpool_t * thread_pool;
    int server_socket;
    struct stream * server_socket_stream;
};

struct socket_stream_handler_context {
    struct poll_thread * poll_thread;
    struct sockaddr_in6 addr;
    tpool_t * thread_pool;
    int socket;
    struct stream * socket_stream;
};

static void socket_stream_handler(
        struct stream * result_stream,
        struct socket_stream_handler_context * context,
        struct poll_thread_event * event
) {
    printf("321 %d\n", context->socket);
    fflush(stdout);
}

static void server_socket_stream_handler(
        struct stream * result_stream,
        struct server_socket_stream_handler_context * context,
        struct poll_thread_event * event
) {
    struct sockaddr_in6 addr;
    socklen_t addrlen = sizeof(addr);
    struct socket_stream_handler_context * socket_stream_handler_context;
    struct stream * socket_stream, * socket_stream_continue;
    int socket, socket_flags;

    socket = accept(context->server_socket, (struct sockaddr *) &addr, &addrlen);
    if (socket < 0) {
        return;
    }

    if (sizeof(addr) != addrlen) {
        return;
    }

    socket_flags = fcntl(socket, F_GETFL, 0);
    if (socket_flags < 0) {
        goto free_socket;
    }

    if (fcntl(socket, F_SETFL, socket_flags | O_NONBLOCK)) {
        goto free_socket;
    }

    socket_stream = stream_new(context->thread_pool);
    if (!socket_stream) {
        goto free_socket;
    }

    if (poll_thread_register(
            context->poll_thread,
            socket,
            POLLIN,
            socket_stream
    )) {
        goto free_socket_stream;
    }

    socket_stream_handler_context = malloc(sizeof(struct socket_stream_handler_context));
    socket_stream_handler_context->poll_thread = context->poll_thread;
    socket_stream_handler_context->thread_pool = context->thread_pool;
    socket_stream_handler_context->socket = socket;
    socket_stream_handler_context->socket_stream = socket_stream;
    socket_stream_handler_context->addr = addr;
    if (!socket_stream_handler_context) {
        goto free_poll_thread_register;
    }

    socket_stream_continue = stream_subscribe(socket_stream,
            socket_stream_handler_context, socket_stream_handler);

    if (!socket_stream_continue) {
        goto free_socket_stream_handler_context;
    }

    printf("123 %d\n", socket);
    fflush(stdout);

    if (poll_thread_register(context->poll_thread, context->server_socket, POLLIN,
            context->server_socket_stream)) {
        goto free_socket_stream_handler_context;
    }

    return;

free_socket_stream_handler_context:
    free(socket_stream_handler_context);

free_poll_thread_register:
    poll_thread_unregister(context->poll_thread, socket, POLLIN);

free_socket_stream:
    stream_delete(socket_stream);

free_socket:
    close(socket);
}

int server_main(struct server_config config) {
    struct server_socket_stream_handler_context * server_socket_stream_handler_context;
    struct stream * server_socket_stream, * server_socket_stream_continue;
    struct poll_thread * poll_thread;
    int server_socket, ret = 0;
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

    poll_thread = poll_thread_new();
    if (!poll_thread) {
        ret = -errno;
        goto free_server_socket;
    }

    thread_pool = tpool_create(8);

    server_socket_stream = stream_new(thread_pool);
    if (!server_socket_stream) {
        ret = -errno;
        goto free_thread_pool;
    }

    server_socket_stream_handler_context = malloc(sizeof(struct server_socket_stream_handler_context));
    if (!server_socket_stream_handler_context) {
        ret = -errno;
        goto free_server_socket_stream;
    }

    server_socket_stream_handler_context->poll_thread = poll_thread;
    server_socket_stream_handler_context->thread_pool = thread_pool;
    server_socket_stream_handler_context->server_socket = server_socket;
    server_socket_stream_handler_context->server_socket_stream = server_socket_stream;

    server_socket_stream_continue = stream_subscribe(server_socket_stream,
            server_socket_stream_handler_context, server_socket_stream_handler);

    if (!server_socket_stream_continue) {
        ret = -errno;
        goto free_server_socket_stream_handler_context;
    }

    ret = poll_thread_register(poll_thread, server_socket, POLLIN, server_socket_stream);
    if (ret) {
        goto free_server_socket_stream_continue;
    }

    ret = poll_thread_run(poll_thread);

free_server_socket_stream_continue:
    stream_delete(server_socket_stream_continue);

free_server_socket_stream_handler_context:
    free(server_socket_stream_handler_context);

free_server_socket_stream:
    stream_delete(server_socket_stream);

free_thread_pool:
    tpool_destroy(thread_pool);

/* free_poll_thread: */
    poll_thread_delete(poll_thread);

free_server_socket:
    close(server_socket);

end:
    return ret;
}
