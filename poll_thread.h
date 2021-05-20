#pragma once

#include <poll.h>

#include "promise.h"


struct poll_thread;

struct poll_thread_event {
    int fd;
    short events;
};

struct poll_thread * poll_thread_new(tpool_t * thread_pool);
void poll_thread_delete(struct poll_thread * poll_thread);

struct promise * poll_thread_register(struct poll_thread * poll_thread, int fd, short events);
int poll_thread_unregister(struct poll_thread * poll_thread, struct promise * promise);

int poll_thread_run(struct poll_thread * poll_thread);
