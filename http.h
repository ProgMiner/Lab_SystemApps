#pragma once

#include "buffer.h"


struct http_headers;

struct http_request;
struct http_request_parser;

enum http_request_parser_result {
    HTTP_REQUEST_PARSER_RESULT_DONE,
    HTTP_REQUEST_PARSER_RESULT_MORE,
    HTTP_REQUEST_PARSER_RESULT_INVAL,
    HTTP_REQUEST_PARSER_RESULT_ERROR
};

struct http_request * http_request_new();
void http_request_delete(struct http_request * request);

const char * http_request_method(struct http_request * request);
const char * http_request_path(struct http_request * request);
const char * http_request_version(struct http_request * request);

struct http_headers * http_request_headers(struct http_request * request);
const uint8_t * http_request_body(struct http_request * request, size_t * length);

const char * http_headers_get(struct http_headers * headers, const char * name);
int http_headers_set(struct http_headers ** headers, const char * name, const char * value);

struct http_request_parser * http_request_parser_new(struct http_request * result);
void http_request_parser_delete(struct http_request_parser * parser);

enum http_request_parser_result http_request_parser_parse(
        struct http_request_parser * parser,
        struct buffer * buffer
);
void http_request_parser_reset(struct http_request_parser * parser);
