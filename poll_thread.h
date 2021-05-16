#pragma once

#include <poll.h>

#include "stream.h"


struct poll_thread;

struct poll_thread_event {
    int fd;
    short events;
};

struct poll_thread * poll_thread_new();
void poll_thread_delete(struct poll_thread * poll_thread);

int poll_thread_register(
        struct poll_thread * poll_thread,
        int fd,
        short events,
        struct stream * stream
);

int poll_thread_unregister(struct poll_thread * poll_thread, int fd, short events);

int poll_thread_run(struct poll_thread * poll_thread);
