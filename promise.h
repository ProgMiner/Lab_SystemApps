#pragma once

#include "thread_pool.h"


#define promise_then(_promise, _context, _handler, _context_free) \
        promise_then_(_promise, _context, (promise_handler) _handler, _context_free)

#define promise_handle(_promise, _context, _handler, _context_free) \
        promise_handle_(_promise, _context, (promise_handler) _handler, _context_free)


struct promise;

struct promise_handler_result {
    void * value;
    void (*value_free)(void *);
};

typedef struct promise_handler_result (*promise_handler)(void *, void *);

struct promise * promise_new(tpool_t * thread_pool);
void promise_delete(struct promise * promise);

int promise_resolve(struct promise * promise, void * value, void (* value_free)(void *));
struct promise * promise_then_(
        struct promise * promise,
        void * context,
        promise_handler handler,
        void (* context_free)(void *)
);

int promise_handle_(
        struct promise * promise,
        void * context,
        promise_handler handler,
        void (* context_free)(void *)
);

struct promise_handler_result promise_handler_result(void * value, void (* value_free)(void *));
