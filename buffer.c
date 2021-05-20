#include "buffer.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


#define BUFFER_READ_FD_BUF_LENGTH (1024)


struct buffer {
    size_t length;
    size_t capacity;
    uint8_t * content;
};

struct buffer * buffer_new(size_t capacity) {
    struct buffer * buffer = malloc(sizeof(struct buffer));

    if (!buffer) {
        errno = ENOMEM;
        return NULL;
    }

    buffer->length = 0;
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

size_t buffer_length(struct buffer * buffer) {
    if (!buffer) {
        return -1;
    }

    return buffer->length;
}

uint8_t * buffer_content(struct buffer * buffer) {
    if (!buffer) {
        return NULL;
    }

    return buffer->content;
}

static int buffer_reserve(struct buffer * buffer, size_t length) {
    size_t new_length, new_capacity;
    uint8_t * new_content;

    if (!buffer) {
        return -EINVAL;
    }

    new_length = buffer->length + length;
    new_capacity = buffer->capacity;

    while (new_capacity < new_length) {
        new_capacity *= 2;
    }

    if (new_capacity == buffer->capacity) {
        return 0;
    }

    new_content = realloc(buffer->content, sizeof(uint8_t) * new_capacity);
    if (!new_content) {
        return -ENOMEM;
    }

    buffer->capacity = new_capacity;
    buffer->content = new_content;
    return 0;
}

static void buffer_do_append_unchecked(
        struct buffer * buffer,
        uint8_t * bytes,
        size_t bytes_length
) {
    memcpy(buffer->content + buffer->length, bytes, bytes_length);
    buffer->length += bytes_length;
}

int buffer_append(struct buffer * buffer, uint8_t * bytes, size_t bytes_length) {
    int ret = buffer_reserve(buffer, bytes_length);

    if (ret) {
        return ret;
    }

    buffer_do_append_unchecked(buffer, bytes, bytes_length);
    return 0;
}

int buffer_drop_to(struct buffer * buffer, size_t position) {
    if (!buffer) {
        return -EINVAL;
    }

    buffer->length -= position;
    memmove(buffer->content, buffer->content + position, buffer->length);
    return 0;
}

int buffer_read_fd(struct buffer * buffer, int fd) {
    uint8_t buf[BUFFER_READ_FD_BUF_LENGTH];
    size_t old_length;
    int ret = 0;

    if (!buffer) {
        return -EINVAL;
    }

    old_length = buffer->length;

    do {
        ret = buffer_reserve(buffer, BUFFER_READ_FD_BUF_LENGTH);
        if (ret) {
            return ret;
        }

        if ((ret = read(fd, buf, BUFFER_READ_FD_BUF_LENGTH)) > 0) {
            buffer_do_append_unchecked(buffer, buf, ret);
        }
    } while (ret == BUFFER_READ_FD_BUF_LENGTH);

    return ret < 0 ? ret : buffer->length - old_length;
}
