#include "io_utils.h"

#include <errno.h>
#include <stdlib.h>


struct read_chunk_handler_context {
    struct buffer * buffer;
    int fd;
};

static struct promise_handler_result read_chunk_handler(
        struct read_chunk_handler_context * context,
        struct poll_thread_event * event
) {
    /* TODO handle errors */
    buffer_read_fd(context->buffer, context->fd);
    return promise_handler_result(context->buffer, NULL);
}

struct promise * io_utils_read_chunk(
        struct poll_thread * poll_thread,
        int fd,
        struct buffer * buffer
) {
    struct read_chunk_handler_context * context;
    struct promise * event_promise, * promise = NULL;

    event_promise = poll_thread_register(poll_thread, fd, POLLIN);
    if (!event_promise) {
        goto end;
    }

    context = malloc(sizeof(struct read_chunk_handler_context));
    if (!context) {
        errno = ENOMEM;
        goto free_poll_thread_register;
    }

    context->buffer = buffer;
    context->fd = fd;

    promise = promise_then(event_promise, context, read_chunk_handler, free);
    if (!promise) {
        errno = ENOMEM;
        goto free_context;
    }

    goto end;

free_context:
    free(context);

free_poll_thread_register:
    poll_thread_unregister(poll_thread, event_promise);
    promise_delete(event_promise);

end:
    return promise;
}

struct promise * io_utils_read_line(
        struct poll_thread * poll_thread,
        int fd,
        struct buffer * buffer
);
