#include "buffer.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


struct buffer {
    size_t position;
    size_t limit;
    size_t capacity;
    uint8_t * content;
};

struct buffer * buffer_new(size_t capacity) {
    struct buffer * buffer = malloc(sizeof(struct buffer));

    if (!buffer) {
        errno = ENOMEM;
        return NULL;
    }

    buffer->position = 0;
    buffer->limit = capacity;
    buffer->capacity = capacity;
    buffer->content = malloc(sizeof(uint8_t) * capacity);
    if (!buffer->content) {
        free(buffer);

        errno = ENOMEM;
        return NULL;
    }

    return buffer;
}

void buffer_delete(struct buffer * buffer) {
    if (!buffer) {
        return;
    }

    free(buffer->content);
    free(buffer);
}

size_t buffer_position(struct buffer * buffer) {
    if (!buffer) {
        return 0;
    }

    return buffer->position;
}

size_t buffer_limit(struct buffer * buffer) {
    if (!buffer) {
        return 0;
    }

    return buffer->limit;
}

size_t buffer_capacity(struct buffer * buffer) {
    if (!buffer) {
        return 0;
    }

    return buffer->capacity;
}

size_t buffer_remaining(struct buffer * buffer) {
    if (!buffer) {
        return 0;
    }

    return buffer->capacity - buffer->position;
}

uint8_t * buffer_content(struct buffer * buffer) {
    if (!buffer) {
        return NULL;
    }

    return buffer->content;
}

uint8_t * buffer_remaining_content(struct buffer * buffer) {
    if (!buffer) {
        return NULL;
    }

    return buffer->content + buffer->position;
}

void buffer_flip(struct buffer * buffer) {
    if (!buffer) {
        return;
    }

    buffer->limit = buffer->position;
    buffer->position = 0;
}

void buffer_drop_start(struct buffer * buffer) {
    if (!buffer) {
        return;
    }

    memmove(buffer->content, buffer->content + buffer->position, buffer->limit - buffer->position);
    buffer->position = buffer->limit - buffer->position;
    buffer->limit = buffer->capacity;
}

int buffer_read_fd(struct buffer * buffer, int fd) {
    size_t remaining, old_pos;
    int ret = 0;

    if (!buffer) {
        return -EINVAL;
    }

    remaining = buffer->limit - buffer->position;
    if (remaining == 0) {
        return 0;
    }

    old_pos = buffer->position;
    if ((ret = read(fd, buffer->content + buffer->position, remaining)) > 0) {
        buffer->position += ret;
    }

    return ret < 0 ? ret : buffer->position - old_pos;
}
