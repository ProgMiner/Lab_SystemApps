#pragma once

#include "poll_thread.h"
#include "buffer.h"


struct promise * io_utils_read_chunk(
        struct poll_thread * poll_thread,
        int fd,
        struct buffer * buffer
);

struct promise * io_utils_read_line(
        struct poll_thread * poll_thread,
        int fd,
        struct buffer * buffer
);
