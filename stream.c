#include "stream.h"

#include <stdlib.h>


struct stream {
    struct stream * next;

    stream_handler handler;
    void * handler_context;
    struct stream * handler_stream;
};

struct stream * stream_new() {
    struct stream * stream = malloc(sizeof(struct stream));

    if (stream) {
        stream->next = NULL;
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

void stream_push(struct stream * stream, void * value) {
    while (stream) {
        if (stream->handler) {
            /* TODO add thread pool and reference counting */
            stream->handler(stream->handler_stream, stream->handler_context, value);
        }

        stream = stream->next;
    }
}

struct stream * stream_subscribe_(struct stream * stream, void * context, stream_handler handler) {
    struct stream * handler_stream = stream_new();

    if (!handler_stream) {
        return NULL;
    }

    while (stream) {
        if (!stream->handler) {
            break;
        }

        if (!stream->next) {
            stream->next = stream_new();

            if (!stream->next) {
                free(handler_stream);
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
