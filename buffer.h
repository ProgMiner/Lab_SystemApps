#pragma once

#include <stddef.h>
#include <stdint.h>


struct buffer;

struct buffer * buffer_new(size_t capacity);
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

int buffer_read_fd(struct buffer * buffer, int fd);
