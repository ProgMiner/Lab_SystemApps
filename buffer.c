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

struct buffer * buffer_shadow(struct buffer * buffer) {
    struct buffer * shadow;

    if (!buffer) {
        return NULL;
    }

    shadow = malloc(sizeof(struct buffer));

    if (!shadow) {
        errno = ENOMEM;
        return NULL;
    }

    shadow->position = 0;
    shadow->limit = buffer->position;
    shadow->capacity = buffer->capacity;
    shadow->content = buffer->content;

    return shadow;
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

    return buffer->limit - buffer->position;
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

void buffer_move(struct buffer * buffer, long offset) {
    if (!buffer) {
        return;
    }

    if (offset < 0 && -offset > buffer->position) {
        buffer->position = 0;
        return;
    }

    if (offset > buffer->limit - buffer->position) {
        buffer->position = buffer->limit;
        return;
    }

    buffer->position += offset;
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

ssize_t buffer_write(struct buffer * buffer, uint8_t * content, size_t content_length) {
    size_t remaining, n;

    if (!buffer) {
        return -EINVAL;
    }

    remaining = buffer->limit - buffer->position;
    if (remaining == 0) {
        return 0;
    }

    n = remaining < content_length ? remaining : content_length;

    memcpy(buffer->content + buffer->position, content, n);
    buffer->position += n;

    return n;
}

ssize_t buffer_write_shadow(struct buffer * buffer, struct buffer * shadow) {
    ssize_t diff = buffer->position - shadow->position;

    shadow->limit = buffer->position;

    return diff;
}

ssize_t buffer_read_fd(struct buffer * buffer, int fd) {
    size_t remaining;
    int ret = 0;

    if (!buffer) {
        return -EINVAL;
    }

    remaining = buffer->limit - buffer->position;
    if (remaining == 0) {
        return 0;
    }

    if ((ret = read(fd, buffer->content + buffer->position, remaining)) > 0) {
        buffer->position += ret;
    }

    return ret;
}

ssize_t buffer_write_fd(struct buffer * buffer, int fd) {
    size_t remaining;
    int ret = 0;

    if (!buffer) {
        return -EINVAL;
    }

    remaining = buffer->limit - buffer->position;
    if (remaining == 0) {
        return 0;
    }

    if ((ret = write(fd, buffer->content + buffer->position, remaining)) > 0) {
        buffer->position += ret;
    }

    return ret;
}
