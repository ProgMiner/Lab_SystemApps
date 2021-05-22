#pragma once

#include <poll.h>

#include "thread_pool.h"


#define poll_thread_handler(_context, _function) \
        poll_thread_handler_(_context, (poll_thread_handler_function *) _function)


struct poll_thread;

struct poll_thread_event {
    int descriptor;

    int fd;
    short events;
};

typedef int poll_thread_handler_function(void *, struct poll_thread_event);

struct poll_thread_handler {
    void * context;
    poll_thread_handler_function * handler;
};

struct poll_thread * poll_thread_new(tpool_t * thread_pool);
void poll_thread_delete(struct poll_thread * poll_thread);

int poll_thread_register(
        struct poll_thread * poll_thread,
        int fd,
        short events,
        struct poll_thread_handler handler
);
int poll_thread_unregister(struct poll_thread * poll_thread, int descriptor);
int poll_thread_continue(struct poll_thread * poll_thread, int descriptor);

int poll_thread_run(struct poll_thread * poll_thread);
int poll_thread_stop(struct poll_thread * poll_thread);

static inline struct poll_thread_handler poll_thread_handler_(
        void * context,
        poll_thread_handler_function * function
) {
    struct poll_thread_handler handler;

    handler.context = context;
    handler.handler = function;
    return handler;
}
