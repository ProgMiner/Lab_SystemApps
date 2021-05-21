#include "io_utils.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>


struct read_chunk_handler_context {
    struct buffer * buffer;
    int fd;
};

struct read_line_handler_context {
    struct poll_thread * poll_thread;
    struct promise * promise;
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

static struct promise_handler_result read_line_handler_next_handler(
        struct promise * promise,
        void * value
) {
    promise_delete(promise);
    return promise_handler_result(NULL, NULL);
}

static struct promise_handler_result read_line_handler(
        struct read_line_handler_context * context,
        struct buffer * buffer
) {
    char * nl_addr = strchr((char *) buffer_content(buffer), '\n');
    struct promise * next_promise;
    size_t length;
    char * result;

    if (nl_addr) {
        length = nl_addr - (char *) buffer_content(buffer);

        if (buffer_content(buffer)[length - 1] == '\r') {
            --length;
        }

        result = malloc(sizeof(char) * (length + 1));
        if (!result) {
            return promise_handler_result(NULL, NULL);
        }

        strncpy(result, (char *) buffer_content(buffer), length);
        result[length] = '\0';

        promise_resolve(context->promise, result, free);

        free(context);
        return promise_handler_result(NULL, NULL);
    }

    next_promise = promise_then(
            io_utils_read_chunk(context->poll_thread, context->fd, buffer),
            context,
            read_line_handler,
            NULL
    );

    if (!next_promise) {
        return promise_handler_result(NULL, NULL);
    }

    errno = -promise_handle(next_promise, next_promise, read_line_handler_next_handler, NULL);
    return promise_handler_result(NULL, NULL);
}

struct promise * io_utils_read_line(
        struct tpool * thread_pool,
        struct poll_thread * poll_thread,
        int fd,
        struct buffer * buffer
) {
    struct read_line_handler_context * context = malloc(sizeof(struct read_line_handler_context));
    if (!context) {
        errno = ENOMEM;
        goto end;
    }

    context->poll_thread = poll_thread;
    context->buffer = buffer;
    context->fd = fd;

    context->promise = promise_new(thread_pool);
    if (!context->promise) {
        errno = ENOMEM;
        goto free_context;
    }

    read_line_handler(context, buffer);
    goto end;

free_context:
    free(context);
    return NULL;

end:
    return context->promise;
}
