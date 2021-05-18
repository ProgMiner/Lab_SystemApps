#include "stream.h"

#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>


struct stream {
    struct stream * next;

    tpool_t * thread_pool;

    stream_handler handler;
    void * handler_context;
    struct stream * handler_stream;
};

struct stream_push_task_context_value {
    int references;
    bool do_free;
    void * value;
};

struct stream_push_task_context {
    struct stream_push_task_context_value * value;

    stream_handler handler;
    void * handler_context;
    struct stream * handler_stream;
};

struct stream * stream_new(tpool_t * thread_pool) {
    struct stream * stream = malloc(sizeof(struct stream));

    if (stream) {
        stream->next = NULL;
        stream->thread_pool = thread_pool;
        stream->handler = NULL;
    }

    return stream;
}

void stream_delete(struct stream * stream) {
    struct stream * next = stream;

    while (next) {
        stream = next;
        next = stream->next;

        free(stream);
    }
}

static void stream_push_task(struct stream_push_task_context * context) {
    context->handler(context->handler_stream, context->handler_context, context->value->value);

    --context->value->references;
    if (context->value->references == 0) {
        if (context->value->do_free) {
            free(context->value->value);
        }

        free(context->value);
    }

    free(context);
}

int stream_push(struct stream * stream, void * value, bool do_free) {
    struct stream_push_task_context_value * task_context_value;
    struct stream_push_task_context * task_context;
    tpool_t * thread_pool;

    if (!stream) {
        return 0;
    }

    thread_pool = stream->thread_pool;

    task_context_value = malloc(sizeof(struct stream_push_task_context_value));
    if (!task_context_value) {
        return -errno;
    }

    task_context_value->do_free = do_free;
    task_context_value->references = 0;
    task_context_value->value = value;

    while (stream) {
        if (stream->handler) {
            task_context = malloc(sizeof(struct stream_push_task_context));

            if (!task_context) {
                return -errno;
            }

            ++task_context_value->references;

            task_context->value = task_context_value;
            task_context->handler = stream->handler;
            task_context->handler_context = stream->handler_context;
            task_context->handler_stream = stream->handler_stream;

            tpool_add_work(thread_pool, (thread_func_t) stream_push_task, task_context, false);
        }

        stream = stream->next;
    }

    return 0;
}

struct stream * stream_subscribe_(struct stream * stream, void * context, stream_handler handler) {
    struct stream * handler_stream;
    tpool_t * thread_pool;

    if (!stream) {
        return NULL;
    }

    thread_pool = stream->thread_pool;
    handler_stream = stream_new(thread_pool);
    if (!handler_stream) {
        return NULL;
    }

    while (stream) {
        if (!stream->handler) {
            break;
        }

        if (!stream->next) {
            stream->next = stream_new(thread_pool);

            if (!stream->next) {
                stream_delete(handler_stream);
                return NULL;
            }
        }

        stream = stream->next;
    }

    stream->handler = handler;
    stream->handler_context = context;
    stream->handler_stream = handler_stream;

    return handler_stream;
}

struct stream * stream_unsubscribe_(struct stream * stream, void * context, stream_handler handler) {
    struct stream * handler_stream;

    if (!stream) {
        return NULL;
    }

    while (stream) {
        if (stream->handler_context == context && stream->handler == handler) {
            break;
        }

        stream = stream->next;
    }

    handler_stream = stream->handler_stream;

    stream->handler = NULL;
    stream->handler_context = NULL;
    stream->handler_stream = NULL;

    return handler_stream;
}
