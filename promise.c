#include "promise.h"

#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>


struct promise_handler_list {
    struct promise_handler_list * next;

    promise_handler handler;

    void * context;
    void (*context_free)(void *);

    struct promise * result_promise;
};

struct promise {
    void * value;
    void (*value_free)(void *);

    tpool_t * thread_pool;
    struct promise_handler_list * handlers;
};

struct promise_notify_handlers_task_context {
    void * value;

    promise_handler handler;

    void * context;
    struct promise * result_promise;
};

struct promise * promise_new(tpool_t * thread_pool) {
    struct promise * promise = malloc(sizeof(struct promise));

    if (promise) {
        promise->value = NULL;
        promise->thread_pool = thread_pool;
        promise->handlers = NULL;
    }

    return promise;
}

void promise_delete(struct promise * promise) {
    struct promise_handler_list * handlers, * handlers_next = promise->handlers;

    if (promise->value_free) {
        promise->value_free(promise->value);
    }

    while (handlers_next) {
        handlers = handlers_next;
        handlers_next = handlers->next;

        if (handlers->context_free) {
            handlers->context_free(handlers->context);
        }

        free(handlers);
    }

    free(promise);
}

static void promise_notify_handlers_task(struct promise_notify_handlers_task_context * context) {
    struct promise_handler_result result = context->handler(context->context, context->value);

    if (context->result_promise) {
        promise_resolve(context->result_promise, result.value, result.value_free);
    }

    free(context);
}

static int promise_notify_handlers(
        struct promise * promise,
        struct promise_handler_list * handlers
) {
    struct promise_notify_handlers_task_context * context;

    if (!promise || !promise->value) {
        return -EINVAL;
    }

    if (!handlers) {
        return 0;
    }

    for (; handlers; handlers = handlers->next) {
        context = malloc(sizeof(struct promise_notify_handlers_task_context));
        if (!context) {
            return -ENOMEM;
        }

        context->value = promise->value;
        context->handler = handlers->handler;
        context->context = handlers->context;
        context->result_promise = handlers->result_promise;

        tpool_add_work(
                promise->thread_pool,
                (thread_func_t) promise_notify_handlers_task,
                context,
                false
        );
    }

    return 0;
}

int promise_resolve(struct promise * promise, void * value, void (* value_free)(void *)) {
    if (!promise || promise->value) {
        return -EINVAL;
    }

    promise->value = value;
    promise->value_free = value_free;
    return promise_notify_handlers(promise, promise->handlers);
}

int promise_do_then(
        struct promise * promise,
        void * context,
        promise_handler handler,
        void (* context_free)(void *),
        struct promise * result_promise
) {
    struct promise_handler_list * handlers, * new_handlers;
    int ret = 0;

    if (!promise) {
        ret = -EINVAL;
        goto end;
    }

    new_handlers = malloc(sizeof(struct promise_handler_list));
    if (!new_handlers) {
        ret = -ENOMEM;
        goto end;
    }

    new_handlers->next = NULL;
    new_handlers->handler = handler;
    new_handlers->context = context;
    new_handlers->context_free = context_free;
    new_handlers->result_promise = result_promise;

    if (promise->value) {
        if ((ret = promise_notify_handlers(promise, new_handlers))) {
            goto free_new_handlers;
        }
    }

    if (promise->handlers) {
        for (handlers = promise->handlers; handlers->next; handlers = handlers->next);

        handlers->next = new_handlers;
    } else {
        promise->handlers = new_handlers;
    }

    goto end;

free_new_handlers:
    free(new_handlers);

end:
    return ret;
}

struct promise * promise_then_(
        struct promise * promise,
        void * context,
        promise_handler handler,
        void (* context_free)(void *)
) {
    struct promise * result_promise;
    int err;

    if (!promise) {
        errno = EINVAL;
        goto bad_end;
    }

    result_promise = promise_new(promise->thread_pool);
    if (!result_promise) {
        errno = ENOMEM;
        goto bad_end;
    }

    err = promise_do_then(promise, context, handler, context_free, result_promise);
    if (err) {
        errno = -err;
        goto bad_end;
    }

    return result_promise;

bad_end:
    return NULL;
}

int promise_handle_(
        struct promise * promise,
        void * context,
        promise_handler handler,
        void (* context_free)(void *)
) {
    if (!promise) {
        return -EINVAL;
    }

    return promise_do_then(promise, context, handler, context_free, NULL);
}

struct promise_handler_result promise_handler_result(void * value, void (* value_free)(void *)) {
    struct promise_handler_result result;

    result.value = value;
    result.value_free = value_free;

    return result;
}
