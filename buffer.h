#pragma once

#include <stddef.h>
#include <stdint.h>


#define BUFFER_DEFAULT_CAPACITY (4096)


struct buffer;

struct buffer * buffer_new(size_t capacity);
void buffer_delete(struct buffer * buffer);

size_t buffer_length(struct buffer * buffer);
uint8_t * buffer_content(struct buffer * buffer);

int buffer_append(struct buffer * buffer, uint8_t * bytes, size_t bytes_length);
int buffer_drop_to(struct buffer * buffer, size_t position);

int buffer_read_fd(struct buffer * buffer, int fd);
