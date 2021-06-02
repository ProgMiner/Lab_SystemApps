#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>


const char * strnstr(const char * haystack, const char * needle, size_t length);
char * strnsextract(const char * source, size_t length, const char * end);
char * strnextract(const char * source, size_t length, char end);
char * strtrim(char * value);

bool strstartswith(const char * haystack, const char * needle);
bool strendswith(const char * haystack, const char * needle);

char * get_directory(const char * path);
char * get_basename(const char * path);

bool is_extension(const char * basename, const char * ext);
