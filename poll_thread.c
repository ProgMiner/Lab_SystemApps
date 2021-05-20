#include "poll_thread.h"

#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <sched.h>


/* I think 250 ms is optimal in order to give time to events for arriving
 * and to not block register functions
 */
#define POLL_WAIT_TIME (250)


struct poll_thread_fd_item {
    struct pollfd * fd;
    struct promise * promise;
};

struct poll_thread {
    unsigned int amount;
    unsigned int capacity;

    struct pollfd * fds;
    struct poll_thread_fd_item * fd_items;

    tpool_t * thread_pool;
    pthread_mutex_t mutex;
};

struct poll_thread * poll_thread_new(tpool_t * thread_pool) {
    struct poll_thread * poll_thread = malloc(sizeof(struct poll_thread));
    int err;

    if (!poll_thread) {
        goto end;
    }

    poll_thread->amount = 0;
    poll_thread->capacity = 4;
    poll_thread->thread_pool = thread_pool;
    poll_thread->fds = malloc(sizeof(struct pollfd) * 4);
    poll_thread->fd_items = malloc(sizeof(struct poll_thread_fd_item) * 4);

    if (!poll_thread->fds || !poll_thread->fd_items) {
        goto free_poll_thread;
    }

    err = pthread_mutex_init(&(poll_thread->mutex), NULL);
    if (err) {
        goto free_poll_thread;
    }

    goto end;

free_poll_thread:
    free(poll_thread->fds);
    free(poll_thread->fd_items);
    free(poll_thread);

end:
    return poll_thread;
}

void poll_thread_delete(struct poll_thread * poll_thread) {
    if (!poll_thread) {
        return;
    }

    pthread_mutex_destroy(&(poll_thread->mutex));

    free(poll_thread->fds);
    free(poll_thread->fd_items);
    free(poll_thread);
}

struct promise * poll_thread_register(struct poll_thread * poll_thread, int fd, short events) {
    struct poll_thread_fd_item * new_fd_items;
    unsigned int new_capacity;
    struct promise * promise;
    struct pollfd * new_fds;
    int ret = 0;

    promise = promise_new(poll_thread->thread_pool);
    if (!promise) {
        ret = -ENOMEM;
        goto end;
    }

    ret = pthread_mutex_lock(&(poll_thread->mutex));
    if (ret) {
        goto free_promise;
    }

    if (poll_thread->capacity == poll_thread->amount) {
        new_capacity = poll_thread->capacity * 2;

        new_fds = realloc(poll_thread->fds, sizeof(struct pollfd) * new_capacity);
        if (new_fds) {
            poll_thread->fds = new_fds;
        } else {
            ret = -ENOMEM;
            goto unlock_mutex;
        }

        new_fd_items = realloc(poll_thread->fd_items,
                sizeof(struct poll_thread_fd_item *) * new_capacity);
        if (new_fd_items) {
            poll_thread->fd_items = new_fd_items;
        } else {
            ret = -ENOMEM;
            goto unlock_mutex;
        }

        poll_thread->capacity = new_capacity;
    }

    poll_thread->fds[poll_thread->amount].fd = fd;
    poll_thread->fds[poll_thread->amount].events = events;
    poll_thread->fds[poll_thread->amount].revents = 0;
    poll_thread->fd_items[poll_thread->amount].fd = poll_thread->fds + poll_thread->amount;
    poll_thread->fd_items[poll_thread->amount].promise = promise;
    ++poll_thread->amount;

    /* ret = 0 here */

unlock_mutex:
    pthread_mutex_unlock(&(poll_thread->mutex));

free_promise:
    if (ret) {
        free(promise);
        promise = NULL;
    }

end:
    if (ret) {
        errno = -ret;
    }

    return promise;
}

int poll_thread_unregister(struct poll_thread * poll_thread, struct promise * promise) {
    unsigned int i;
    int ret = 0;

    ret = pthread_mutex_lock(&(poll_thread->mutex));
    if (ret) {
        goto end;
    }

    for (i = 0; i < poll_thread->amount; ++i) {
        if (poll_thread->fd_items[i].promise != promise) {
            continue;
        }

        --poll_thread->amount;
        poll_thread->fds[i] = poll_thread->fds[poll_thread->amount];
        poll_thread->fd_items[i] = poll_thread->fd_items[poll_thread->amount];
    }

/* unlock_mutex: */
    ret = pthread_mutex_unlock(&(poll_thread->mutex));

end:
    return ret;
}

int poll_thread_run(struct poll_thread * poll_thread) {
    struct poll_thread_fd_item fd_item;
    struct poll_thread_event * event;
    unsigned int i, j;
    struct pollfd fd;
    int ret = 0;

    while (true) {
        ret = pthread_mutex_lock(&(poll_thread->mutex));
        if (ret) {
            goto end;
        }

        ret = poll(poll_thread->fds, poll_thread->amount, POLL_WAIT_TIME);
        if (ret < 0) {
            goto unlock_mutex;
        }

        for (i = 0, j = 0; i < poll_thread->amount && j < ret; ++i) {
            if (!poll_thread->fds[i].revents) {
                continue;
            }

            event = malloc(sizeof(struct poll_thread_event));
            if (!event) {
                ret = -errno;
                goto unlock_mutex;
            }

            fd = poll_thread->fds[i];
            fd_item = poll_thread->fd_items[i];

            /* unregister manually */
            --poll_thread->amount;
            poll_thread->fds[i] = poll_thread->fds[poll_thread->amount];
            poll_thread->fd_items[i] = poll_thread->fd_items[poll_thread->amount];

            event->fd = fd.fd;
            event->events = fd.revents;
            if ((ret = promise_resolve(fd_item.promise, event, free))) {
                goto unlock_mutex;
            }

            ++j;
        }

        ret = pthread_mutex_unlock(&(poll_thread->mutex));
        if (ret) {
            goto end;
        }

        /* yield in order to give to another threads ability to register or unregister fds */
        ret = sched_yield();
        if (ret) {
            goto end;
        }
    }

unlock_mutex:
    pthread_mutex_unlock(&(poll_thread->mutex));

end:
    return ret;
}
