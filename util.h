#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>


const char * strnstr(const char * haystack, const char * needle, size_t length);
char * strnsextract(const char * source, size_t length, const char * end);
char * strnextract(const char * source, size_t length, char end);
char * strtrim(char * value);

bool strstartswith(const char * haystack, const char * needle);
