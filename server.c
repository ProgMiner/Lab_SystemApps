#include "server.h"

#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "poll_thread.h"


struct server_socket_stream_handler_context {
    struct poll_thread * poll_thread;
    int server_socket;
};

void server_socket_stream_handler(
        struct stream * result_stream,
        struct server_socket_stream_handler_context * context,
        struct poll_thread_event * event
) {
    struct sockaddr_in6 addr;
    socklen_t addrlen = sizeof(addr);
    int socket;

    socket = accept(context->server_socket, (struct sockaddr *) &addr, &addrlen);
    if (socket < 0) {
        return;
    }

    printf("123 %d\n", socket);
}

int server_main(struct server_config config) {
    struct server_socket_stream_handler_context server_socket_stream_handler_context;
    struct stream * server_socket_stream, * server_socket_stream_continue;
    struct poll_thread * poll_thread;
    int server_socket, ret = 0;

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

    server_socket_stream = stream_new();
    if (!server_socket_stream) {
        ret = -errno;
        goto free_poll_thread;
    }

    server_socket_stream_handler_context.poll_thread = poll_thread;
    server_socket_stream_handler_context.server_socket = server_socket;
    server_socket_stream_continue = stream_subscribe(server_socket_stream,
            &server_socket_stream_handler_context, server_socket_stream_handler);

    if (!server_socket_stream_continue) {
        ret = -errno;
        goto free_server_socket_stream;
    }

    ret = poll_thread_register(poll_thread, server_socket, POLLIN, server_socket_stream);
    if (ret) {
        goto free_server_socket_stream_continue;
    }

    ret = poll_thread_run(poll_thread);

free_server_socket_stream_continue:
    stream_delete(server_socket_stream_continue);

free_server_socket_stream:
    stream_delete(server_socket_stream);

free_poll_thread:
    poll_thread_delete(poll_thread);

free_server_socket:
    close(server_socket);

end:
    return ret;
}
