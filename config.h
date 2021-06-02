#pragma once

#include "poll_thread.h"


#define config_manager_handler(_function, _context) \
        config_manager_handler_((config_manager_handler_function *) _function, _context)


struct config {
    enum {
        CONFIG_ACTION_MIME,
        CONFIG_ACTION_CGI,
        CONFIG_ACTION_DENY
    } action;

    union {
        const char * interpreter;
        const char * mime_type;
    } value;
};

struct config_manager;

typedef int config_manager_handler_function(void *, struct config);

struct config_manager_handler {
    config_manager_handler_function * handler;
    void * context;
};

struct config_manager * config_manager_new(struct poll_thread * poll_thread, const char * work_dir);
void config_manager_delete(struct config_manager * config_manager);

int config_manager_resolve(
        struct config_manager * config_manager,
        const char * filename,
        struct config_manager_handler handler
);

static inline struct config_manager_handler config_manager_handler_(
    config_manager_handler_function * function,
    void * context
) {
    struct config_manager_handler handler;

    handler.handler = function;
    handler.context = context;

    return handler;
}
