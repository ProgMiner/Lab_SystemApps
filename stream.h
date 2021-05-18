#pragma once

#include "thread_pool.h"


#define stream_subscribe(_stream, _context, _handler) \
        stream_subscribe_(_stream, _context, (stream_handler) _handler)

#define stream_unsubscribe(_stream, _context, _handler) \
        stream_unsubscribe_(_stream, _context, (stream_handler) _handler)


struct stream;

typedef void (*stream_handler)(struct stream *, void *, void *);

struct stream * stream_new(tpool_t * thread_pool);
void stream_delete(struct stream * stream);

int stream_push(struct stream * stream, void * value, bool do_free);
struct stream * stream_subscribe_(struct stream * stream, void * context, stream_handler handler);
struct stream * stream_unsubscribe_(struct stream * stream, void * context, stream_handler handler);
