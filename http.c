#include "http.h"

#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <strings.h>

#include "util.h"


/* 64K */
#define MAX_PARSED_HEADER_CONTENT (65536)


struct http_headers {
    struct http_headers * next;

    char * name;
    char * value;
};

struct http_request {
    char * method;
    char * path;
    char * version;

    struct http_headers * headers;

    uint8_t * body;
    size_t body_length;
};

struct http_request_parser {
    struct http_request * result;

    enum {
        HTTP_REQUEST_PARSER_STATE_METHOD,
        HTTP_REQUEST_PARSER_STATE_PATH,
        HTTP_REQUEST_PARSER_STATE_VERSION,
        HTTP_REQUEST_PARSER_STATE_HEADER_NAME,
        HTTP_REQUEST_PARSER_STATE_HEADER_VALUE,
        HTTP_REQUEST_PARSER_STATE_BODY,
        HTTP_REQUEST_PARSER_STATE_DONE
    } state;

    char * last_header_name;
    char * last_header_value;
    char * last_header_value_position;
    size_t last_header_value_length;

    uint8_t * body_position;
};

struct http_request * http_request_new() {
    return calloc(1, sizeof(struct http_request));
}

static void http_headers_delete(struct http_headers * headers) {
    struct http_headers * next = headers;

    while (next) {
        headers = next;
        next = headers->next;

        free(headers->name);
        free(headers->value);
        free(headers);
    }
}

void http_request_delete(struct http_request * request) {
    if (!request) {
        return;
    }

    free(request->method);
    free(request->path);
    free(request->version);
    http_headers_delete(request->headers);
    free(request->body);
    free(request);
}

const char * http_request_method(struct http_request * request) {
    return request->method;
}

const char * http_request_path(struct http_request * request) {
    return request->path;
}

const char * http_request_version(struct http_request * request) {
    return request->version;
}

struct http_headers * http_request_headers(struct http_request * request) {
    return request->headers;
}

const uint8_t * http_request_body(struct http_request * request, size_t * length) {
    if (length) {
        *length = request->body_length;
    }

    return request->body;
}

const char * http_headers_get(struct http_headers * headers, const char * name) {
    for (; headers; headers = headers->next) {
        if (strcasecmp(headers->name, name) != 0) {
            continue;
        }

        return headers->value;
    }

    return NULL;
}

static void http_headers_remove(struct http_headers ** headers, const char * name) {
    struct http_headers * header, * header_prev = NULL;

    /* assume that headers != NULL && name != NULL */

    for (header = *headers; header; header_prev = header, header = header->next) {
        if (strcmp(header->name, name) != 0) {
            continue;
        }

        if (header_prev) {
            header_prev->next = header->next;
        } else {
            *headers = header->next;
        }

        free(header->name);
        free(header->value);
        free(header);

        header = header_prev;
    }
}

int http_headers_set(struct http_headers ** headers, const char * name, const char * value) {
    struct http_headers * header;
    size_t new_value_length;
    char * new_value;
    int ret = 0;

    if (!headers || !name) {
        return -EINVAL;
    }

    if (!value) {
        http_headers_remove(headers, name);
        return 0;
    }

    new_value_length = strlen(value);
    for (header = *headers; header; header = header->next) {
        if (strcmp(header->name, name) != 0) {
            continue;
        }

        new_value = realloc(header->value, sizeof(char) * (new_value_length + 1));
        if (!new_value) {
            return -ENOMEM;
        }

        strncpy(new_value, value, new_value_length + 1);
        header->value = new_value;
        return 0;
    }

    header = malloc(sizeof(struct http_headers));
    if (!header) {
        return -ENOMEM;
    }

    header->next = *headers;
    header->name = strdup(name);
    if (!header->name) {
        ret = -ENOMEM;
        goto free_header;
    }

    header->value = strdup(value);
    if (!header->value) {
        ret = -ENOMEM;
        goto free_header_name;
    }

    *headers = header;
    goto end;

free_header_name:
    free(header->name);

free_header:
    free(header);

end:
    return ret;
}

struct http_request_parser * http_request_parser_new(struct http_request * result) {
    struct http_request_parser * parser;

    if (!result) {
        errno = EINVAL;
        return NULL;
    }

    parser = malloc(sizeof(struct http_request_parser));
    if (!parser) {
        return NULL;
    }

    parser->result = result;
    parser->state = HTTP_REQUEST_PARSER_STATE_METHOD;
    parser->last_header_name = NULL;
    parser->last_header_value = NULL;
    parser->last_header_value_position = NULL;
    parser->last_header_value_length = 0;
    return parser;
}

void http_request_parser_delete(struct http_request_parser * parser) {
    free(parser->last_header_name);
    free(parser->last_header_value);
    free(parser);
}

static bool http_request_parser_check_version(const char * version) {
    const char * endptr;

    double double_version = strtod(version, (char **) &endptr);
    if (endptr == version) {
        return false;
    }

    switch ((int) (double_version * 10)) {
    case 9:
    case 10:
    case 11:
        return true;
    }

    return false;
}

static int http_request_parser_fill_body_length(struct http_request_parser * parser) {
    const char * content_length = http_headers_get(parser->result->headers, "Content-Length");
    const char * endptr;
    long int n;

    if (!content_length) {
        parser->result->body_length = 0;
        return 0;
    }

    n = strtol(content_length, (char **) &endptr, 10);
    if (endptr == content_length || n < 0 || n > SIZE_MAX) {
        return -EINVAL;
    }

    parser->result->body_length = (size_t) n;
    return 0;
}

enum http_request_parser_result http_request_parser_parse(
        struct http_request_parser * parser,
        struct buffer * buffer
) {
    size_t content_length, tmp_length, new_last_header_value_length, body_remaining;
    char * content, * tmp, * new_last_header_value, * new_last_header_value_position;
    bool header_content_not_ended;
    int err, move;

    if (!parser || !buffer) {
        errno = EINVAL;
        return HTTP_REQUEST_PARSER_RESULT_ERROR;
    }

    while (parser->state != HTTP_REQUEST_PARSER_STATE_DONE) {
        content_length = buffer_remaining(buffer);
        content = (char *) buffer_remaining_content(buffer);
        move = 0;

        switch (parser->state) {
        case HTTP_REQUEST_PARSER_STATE_METHOD:
            tmp = strnextract(content, content_length, ' ');

            if (!tmp) {
                if (buffer_limit(buffer) < buffer_capacity(buffer)) {
                    return HTTP_REQUEST_PARSER_RESULT_MORE;
                } else {
                    return HTTP_REQUEST_PARSER_RESULT_INVAL;
                }
            }

            move = strlen(tmp) + 1;
            parser->result->method = tmp;
            parser->state = HTTP_REQUEST_PARSER_STATE_PATH;
            break;

        case HTTP_REQUEST_PARSER_STATE_PATH:
            tmp = strnextract(content, content_length, ' ');

            if (!tmp) {
                if (buffer_limit(buffer) < buffer_capacity(buffer)) {
                    return HTTP_REQUEST_PARSER_RESULT_MORE;
                } else {
                    return HTTP_REQUEST_PARSER_RESULT_INVAL;
                }
            }

            move = strlen(tmp) + 1;
            parser->result->path = tmp;
            parser->state = HTTP_REQUEST_PARSER_STATE_VERSION;
            break;

        case HTTP_REQUEST_PARSER_STATE_VERSION:
            tmp = strnsextract(content, content_length, "\r\n");

            if (!tmp) {
                if (buffer_limit(buffer) < buffer_capacity(buffer)) {
                    return HTTP_REQUEST_PARSER_RESULT_MORE;
                } else {
                    return HTTP_REQUEST_PARSER_RESULT_INVAL;
                }
            }

            move = strlen(tmp) + 2;

            if (strncmp(tmp, "HTTP/", 5) != 0) {
                free(tmp);
                return HTTP_REQUEST_PARSER_RESULT_INVAL;
            }

            tmp_length = strlen(tmp);
            memmove(tmp, tmp + 5, tmp_length - 4);
            tmp = realloc(tmp, tmp_length - 4);

            if (!http_request_parser_check_version(tmp)) {
                free(tmp);
                return HTTP_REQUEST_PARSER_RESULT_INVAL;
            }

            parser->result->version = tmp;
            parser->state = HTTP_REQUEST_PARSER_STATE_HEADER_NAME;
            break;

        case HTTP_REQUEST_PARSER_STATE_HEADER_NAME:
            tmp = strnextract(content, content_length, ':');

            if (!tmp) {
                tmp = strnsextract(content, content_length, "\r\n");

                if (tmp && strlen(tmp) == 0) {
                    if (!http_request_parser_fill_body_length(parser)) {

                        if (parser->result->body_length > 0) {
                            parser->result->body =
                                    malloc(sizeof(uint8_t) * parser->result->body_length);

                            if (!parser->result->body) {
                                return HTTP_REQUEST_PARSER_RESULT_ERROR;
                            }
                        }

                        move = 2;
                        parser->body_position = parser->result->body;
                        parser->state = HTTP_REQUEST_PARSER_STATE_BODY;
                        break;
                    }

                    return HTTP_REQUEST_PARSER_RESULT_INVAL;
                }

                if (buffer_limit(buffer) < buffer_capacity(buffer)) {
                    return HTTP_REQUEST_PARSER_RESULT_MORE;
                } else {
                    return HTTP_REQUEST_PARSER_RESULT_INVAL;
                }
            }

            move = strlen(tmp) + 1;

            parser->last_header_name = tmp;
            parser->state = HTTP_REQUEST_PARSER_STATE_HEADER_VALUE;
            break;

        case HTTP_REQUEST_PARSER_STATE_HEADER_VALUE:
            tmp = strnsextract(content, content_length, "\r\n");

            if (!tmp) {
                tmp = content;
                tmp_length = content_length;
                header_content_not_ended = true;

                move = tmp_length;
            } else {
                tmp_length = strlen(tmp);
                header_content_not_ended = false;

                move = tmp_length + 2;
            }

            new_last_header_value_length = parser->last_header_value_length + tmp_length;
            if (new_last_header_value_length > MAX_PARSED_HEADER_CONTENT) {
                return HTTP_REQUEST_PARSER_RESULT_INVAL;
            }

            new_last_header_value = realloc(parser->last_header_value,
                    sizeof(uint8_t) * new_last_header_value_length);
            if (!new_last_header_value) {
                return HTTP_REQUEST_PARSER_RESULT_ERROR;
            }

            new_last_header_value_position = new_last_header_value
                    + (parser->last_header_value_position - parser->last_header_value);

            memcpy(new_last_header_value_position, tmp, tmp_length);

            if (header_content_not_ended) {
                buffer_move(buffer, move);
                parser->last_header_value = new_last_header_value;
                parser->last_header_value_position = new_last_header_value_position + tmp_length;
                parser->last_header_value_length = new_last_header_value_length;
                return HTTP_REQUEST_PARSER_RESULT_MORE;
            } else {
                free(tmp);
            }

            err = http_headers_set(&(parser->result->headers), parser->last_header_name,
                    strtrim(new_last_header_value));

            if (err) {
                errno = -err;
                return HTTP_REQUEST_PARSER_RESULT_ERROR;
            }

            parser->last_header_name = NULL;
            parser->last_header_value = NULL;
            parser->last_header_value_position = NULL;
            parser->last_header_value_length = 0;
            parser->state = HTTP_REQUEST_PARSER_STATE_HEADER_NAME;
            break;

        case HTTP_REQUEST_PARSER_STATE_BODY:
            body_remaining = parser->result->body_length
                    - (parser->body_position - parser->result->body);

            if (body_remaining == 0) {
                parser->body_position = NULL;
                parser->state = HTTP_REQUEST_PARSER_STATE_DONE;
                break;
            }

            if (content_length == 0) {
                return HTTP_REQUEST_PARSER_RESULT_MORE;
            }

            body_remaining = content_length < body_remaining ? content_length : body_remaining;
            memcpy(parser->body_position, content, body_remaining);

            move = body_remaining;
            parser->body_position += body_remaining;
            break;

        case HTTP_REQUEST_PARSER_STATE_DONE:
            break;
        }

        buffer_move(buffer, move);
    }

    return HTTP_REQUEST_PARSER_RESULT_DONE;
}
