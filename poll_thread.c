#include "poll_thread.h"

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <sched.h>


/* I think 25 ms is optimal in order to give time to events for arriving
 * and to not block register functions
 */
#define POLL_WAIT_TIME (25)


struct poll_thread_fd_item {
    int descriptor;

    int fd;
    struct poll_thread_handler handler;
};

struct poll_thread {
    bool stopped;
    unsigned int amount;
    unsigned int capacity;
    int last_descriptor;
    int run_retval;

    struct pollfd * fds;
    struct poll_thread_fd_item * fd_items;
    int * index_by_descriptor;

    tpool_t * thread_pool;
    pthread_mutex_t mutex;
};

struct poll_thread_run_task_context {
    struct poll_thread * poll_thread;

    struct poll_thread_handler handler;
    struct poll_thread_event event;
};

struct poll_thread * poll_thread_new(tpool_t * thread_pool) {
    struct poll_thread * poll_thread = malloc(sizeof(struct poll_thread));
    int err;

    if (!poll_thread) {
        goto end;
    }

    poll_thread->stopped = false;
    poll_thread->amount = 0;
    poll_thread->capacity = 4;
    poll_thread->run_retval = 0;
    poll_thread->last_descriptor = -1;
    poll_thread->thread_pool = thread_pool;

    poll_thread->fds = malloc(sizeof(struct pollfd) * 4);
    if (!poll_thread->fds) {
        goto free_poll_thread;
    }

    poll_thread->fd_items = malloc(sizeof(struct poll_thread_fd_item) * 4);
    if (!poll_thread->fd_items) {
        goto free_fds;
    }

    poll_thread->index_by_descriptor = malloc(sizeof(unsigned int) * 4);
    if (!poll_thread->index_by_descriptor) {
        goto free_fd_items;
    }

    err = pthread_mutex_init(&(poll_thread->mutex), NULL);
    if (err) {
        errno = -err;
        goto free_index_by_descriptor;
    }

    goto end;

free_index_by_descriptor:
    free(poll_thread->index_by_descriptor);

free_fd_items:
    free(poll_thread->fd_items);

free_fds:
    free(poll_thread->fds);

free_poll_thread:
    free(poll_thread);
    poll_thread = NULL;

end:
    return poll_thread;
}

void poll_thread_delete(struct poll_thread * poll_thread) {
    if (!poll_thread) {
        return;
    }

    pthread_mutex_destroy(&(poll_thread->mutex));
    free(poll_thread->index_by_descriptor);
    free(poll_thread->fd_items);
    free(poll_thread->fds);
    free(poll_thread);
}

int poll_thread_register(
        struct poll_thread * poll_thread,
        int fd,
        short events,
        struct poll_thread_handler handler
) {
    struct poll_thread_fd_item * new_fd_items;
    int * new_index_by_descriptor;
    unsigned int new_capacity;
    struct pollfd * new_fds;
    int ret = 0, index, err;

    ret = pthread_mutex_lock(&(poll_thread->mutex));
    if (ret) {
        ret = -ret;
        goto end;
    }

    if (poll_thread->amount == INT_MAX) {
        ret = -ERANGE;
        goto unlock_mutex;
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
                sizeof(struct poll_thread_fd_item) * new_capacity);
        if (new_fd_items) {
            poll_thread->fd_items = new_fd_items;
        } else {
            ret = -ENOMEM;
            goto unlock_mutex;
        }

        new_index_by_descriptor = realloc(poll_thread->index_by_descriptor,
                sizeof(int) * new_capacity);
        if (new_index_by_descriptor) {
            poll_thread->index_by_descriptor = new_index_by_descriptor;
        } else {
            ret = -ENOMEM;
            goto unlock_mutex;
        }

        poll_thread->capacity = new_capacity;
    }

    for (ret = 0; ret < poll_thread->amount; ++ret) {
        if (poll_thread->index_by_descriptor[ret] == -1) {
            break;
        }
    }

    index = poll_thread->amount++;
    poll_thread->fds[index].fd = fd;
    poll_thread->fds[index].events = events;
    poll_thread->fds[index].revents = 0;
    poll_thread->fd_items[index].descriptor = ret;
    poll_thread->fd_items[index].fd = fd;
    poll_thread->fd_items[index].handler = handler;
    poll_thread->index_by_descriptor[ret] = index;

    poll_thread->last_descriptor = poll_thread->last_descriptor > ret
        ? poll_thread->last_descriptor
        : ret;

unlock_mutex:
    if ((err = pthread_mutex_unlock(&(poll_thread->mutex)))) {
        ret = -err;
    }

end:
    return ret;
}

int poll_thread_unregister(struct poll_thread * poll_thread, int descriptor) {
    unsigned int index, i;
    int ret = 0;

    ret = pthread_mutex_lock(&(poll_thread->mutex));
    if (ret) {
        ret = -ret;
        goto end;
    }

    if (descriptor < 0 || descriptor > poll_thread->last_descriptor
            || poll_thread->index_by_descriptor[descriptor] == -1) {
        ret = -EINVAL;
        goto unlock_mutex;
    }

    index = poll_thread->index_by_descriptor[descriptor];

    i = poll_thread->amount--;
    poll_thread->fds[index] = poll_thread->fds[i];
    poll_thread->fd_items[index] = poll_thread->fd_items[i];
    poll_thread->index_by_descriptor[poll_thread->fd_items[index].descriptor] = i;
    poll_thread->index_by_descriptor[descriptor] = -1;

    poll_thread->last_descriptor = poll_thread->last_descriptor > descriptor
        ? poll_thread->last_descriptor
        : descriptor;

unlock_mutex:
    ret = -pthread_mutex_unlock(&(poll_thread->mutex));

end:
    return ret;
}

int poll_thread_continue(struct poll_thread * poll_thread, int descriptor) {
    unsigned int index;
    int ret = 0;

    ret = pthread_mutex_lock(&(poll_thread->mutex));
    if (ret) {
        ret = -ret;
        goto end;
    }

    if (descriptor < 0 || descriptor > poll_thread->last_descriptor
            || poll_thread->index_by_descriptor[descriptor] == -1) {
        ret = -EINVAL;
        goto unlock_mutex;
    }

    index = poll_thread->index_by_descriptor[descriptor];
    poll_thread->fds[index].fd = poll_thread->fd_items[index].fd;

unlock_mutex:
    ret = -pthread_mutex_unlock(&(poll_thread->mutex));

end:
    return ret;
}

static void poll_thread_run_task(struct poll_thread_run_task_context * context) {
    int ret = context->handler.handler(
            context->handler.context,
            context->event
    );

    if (ret) {
        if (pthread_mutex_lock(&(context->poll_thread->mutex))) {
            return;
        }

        context->poll_thread->run_retval = ret;
        context->poll_thread->stopped = true;

        pthread_mutex_unlock(&(context->poll_thread->mutex));
    }
}

int poll_thread_run(struct poll_thread * poll_thread) {
    struct poll_thread_run_task_context * task_context;
    unsigned int i, j;
    int ret = 0;

    ret = -pthread_mutex_lock(&(poll_thread->mutex));
    if (ret) {
        goto end;
    }

    poll_thread->stopped = false;

    while (!poll_thread->stopped) {
        ret = poll(poll_thread->fds, poll_thread->amount, POLL_WAIT_TIME);
        if (ret < 0) {
            goto unlock_mutex;
        }

        for (i = 0, j = 0; i < poll_thread->amount && j < ret; ++i) {
            if ((poll_thread->fds[i].revents & poll_thread->fds[i].events) == 0) {
                continue;
            }

            task_context = malloc(sizeof(struct poll_thread_run_task_context));
            if (!task_context) {
                ret = -ENOMEM;
                goto unlock_mutex;
            }

            task_context->poll_thread = poll_thread;
            task_context->event.descriptor = poll_thread->fd_items[i].descriptor;
            task_context->event.fd = poll_thread->fds[i].fd;
            task_context->event.events = poll_thread->fds[i].revents & poll_thread->fds[i].events;
            task_context->handler = poll_thread->fd_items[i].handler;

            poll_thread->fds[i].fd = -1;
            poll_thread->fds[i].revents = 0;

            tpool_add_work(poll_thread->thread_pool, (thread_func_t) poll_thread_run_task,
                    task_context, false);

            ++j;
        }

        ret = -pthread_mutex_unlock(&(poll_thread->mutex));
        if (ret) {
            goto end;
        }

        if (sched_yield()) {
            ret = -errno;
            goto end;
        }

        ret = -pthread_mutex_lock(&(poll_thread->mutex));
        if (ret) {
            goto end;
        }
    }

    ret = poll_thread->run_retval;

    if (!ret) {
        ret = -pthread_mutex_unlock(&(poll_thread->mutex));
        goto end;
    }

unlock_mutex:
    pthread_mutex_unlock(&(poll_thread->mutex));

end:
    return ret;
}
