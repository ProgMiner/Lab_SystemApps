#pragma once


#define stream_subscribe(_stream, _context, _handler) \
        stream_subscribe_(_stream, _context, (stream_handler) _handler)


struct stream;

typedef void (*stream_handler)(struct stream *, void *, void *);

/* TODO add thread pool */
struct stream * stream_new();
void stream_delete(struct stream * stream);

void stream_push(struct stream * stream, void * value);
struct stream * stream_subscribe_(struct stream * stream, void * context, stream_handler handler);
