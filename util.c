#include "util.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <libgen.h>


const char * strnstr(const char * haystack, const char * needle, size_t length) {
    const char * prev_haystack = haystack;
    size_t needle_length;

    if (!haystack || !needle) {
        return NULL;
    }

    needle_length = strlen(needle);
    if (needle_length == 0) {
        return haystack;
    }

    for (prev_haystack = haystack, haystack = memchr(haystack, needle[0], length); haystack;
            prev_haystack = ++haystack, haystack = memchr(haystack, needle[0], length)) {
        length -= haystack - prev_haystack;

        if (length < needle_length) {
            break;
        }

        if (strncmp(haystack, needle, needle_length) == 0) {
            return haystack;
        }
    }

    return NULL;
}

char * strnsextract(const char * source, size_t length, const char * end) {
    const char * end_ptr;
    size_t result_length;
    char * result;

    if (!source || !end) {
        return NULL;
    }

    end_ptr = strnstr(source, end, length);
    if (!end_ptr) {
        return NULL;
    }

    result_length = end_ptr - source;
    result = malloc(sizeof(char) * (result_length + 1));
    if (!result) {
        return NULL;
    }

    strncpy(result, source, result_length + 1);
    result[result_length] = '\0';
    return result;
}

char * strnextract(const char * source, size_t length, char end) {
    char endstr[2];

    endstr[0] = end;
    endstr[1] = '\0';
    return strnsextract(source, length, endstr);
}

char * strtrim(char * value) {
    size_t length;
    char * start;

    if (!value) {
        return NULL;
    }

    for (start = value; start; ++start) {
        if (!isspace(*start)) {
            break;
        }
    }

    for (length = strlen(start); length > 0; --length) {
        if (!isspace(start[length - 1])) {
            break;
        }
    }

    memmove(value, start, length);
    value[length] = '\0';

    return realloc(value, sizeof(char) * (length + 1));
}

bool strstartswith(const char * haystack, const char * needle) {
    size_t haystack_length, needle_length;

    if (!haystack || !needle) {
        return false;
    }

    haystack_length = strlen(haystack);
    needle_length = strlen(needle);

    if (needle_length > haystack_length) {
        return false;
    }

    return strncmp(haystack, needle, needle_length) == 0;
}

bool strendswith(const char * haystack, const char * needle) {
    size_t haystack_length, needle_length;

    if (!haystack || !needle) {
        return false;
    }

    haystack_length = strlen(haystack);
    needle_length = strlen(needle);

    if (needle_length > haystack_length) {
        return false;
    }

    return strncmp(haystack + (haystack_length - needle_length), needle, needle_length) == 0;
}

char * get_directory(const char * path) {
    char * buf, * dir;

    buf = strdup(path);
    if (!buf) {
        return NULL;
    }

    dir = strdup(dirname(buf));
    if (!dir) {
        return NULL;
    }

    free(buf);
    return dir;
}

char * get_basename(const char * path) {
    char * buf, * dir;

    buf = strdup(path);
    if (!buf) {
        return NULL;
    }

    dir = strdup(basename(buf));
    if (!dir) {
        return NULL;
    }

    free(buf);
    return dir;
}

bool is_extension(const char * basename, const char * ext) {
    size_t basename_length, ext_length;

    if (!basename || !ext) {
        return false;
    }

    basename_length = strlen(basename);
    ext_length = strlen(ext);

    if (ext_length > basename_length) {
        return false;
    }

    if (strncasecmp(basename + (basename_length - ext_length), ext, ext_length) != 0) {
        return false;
    }

    return ext[0] == '.' || basename[basename_length - ext_length - 1] == '.';
}
