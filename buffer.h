#pragma once

#include <stddef.h>
#include <stdint.h>
#include <unistd.h>


struct buffer;

struct buffer * buffer_new(size_t capacity);
struct buffer * buffer_shadow(struct buffer * buffer);
void buffer_delete(struct buffer * buffer);

size_t buffer_position(struct buffer * buffer);
size_t buffer_limit(struct buffer * buffer);
size_t buffer_capacity(struct buffer * buffer);
size_t buffer_remaining(struct buffer * buffer);
uint8_t * buffer_content(struct buffer * buffer);
uint8_t * buffer_remaining_content(struct buffer * buffer);

void buffer_move(struct buffer * buffer, long offset);

void buffer_flip(struct buffer * buffer);
void buffer_drop_start(struct buffer * buffer);

ssize_t buffer_write(struct buffer * buffer, uint8_t * content, size_t content_length);

ssize_t buffer_write_shadow(struct buffer * buffer, struct buffer * shadow);

ssize_t buffer_read_fd(struct buffer * buffer, int fd);
ssize_t buffer_write_fd(struct buffer * buffer, int fd);
