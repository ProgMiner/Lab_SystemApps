#pragma once

#include "thread_pool.h"
#include "poll_thread.h"
#include "buffer.h"


struct promise * io_utils_read_chunk(
        struct poll_thread * poll_thread,
        int fd,
        struct buffer * buffer
);

struct promise * io_utils_read_line(
        struct tpool * thread_pool,
        struct poll_thread * poll_thread,
        int fd,
        struct buffer * buffer
);
