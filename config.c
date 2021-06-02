#include "config.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <pthread.h>
#include <regex.h>
#include <fcntl.h>

#include "buffer.h"
#include "util.h"


struct config_rule {
    struct config_rule * next;

    enum {
        CONFIG_RULE_ACTION_MIME,
        CONFIG_RULE_ACTION_CGI,
        CONFIG_RULE_ACTION_DENY
    } action;

    union {
        char * extension;
        regex_t regexp;
    } pattern;

    char * value;
};

struct config_manager_cache {
    struct config_manager_cache * next;

    char * dir_path;
    time_t update;

    struct config_rule * rules;
};

struct config_manager {
    struct poll_thread * poll_thread;
    char * work_dir;

    pthread_rwlock_t cache_lock;
    struct config_manager_cache * cache;
};

struct config_manager_traverse_handler_context {
    struct config_manager * config_manager;

    char * dir;
    char * basename;
    struct config_manager_handler handler;

    struct buffer * buffer;
    struct config_rule * rules;
};

static const char * const config_filename = ".lab3.conf";

static const struct config default_config = { CONFIG_ACTION_MIME, { (void *) "text/plain" } };

struct config_manager * config_manager_new(
        struct poll_thread * poll_thread,
        const char * work_dir
) {
    struct config_manager * config_manager;
    int err;

    config_manager = malloc(sizeof(struct config_manager));
    if (!config_manager) {
        return NULL;
    }

    config_manager->work_dir = strdup(work_dir);
    if (!config_manager->work_dir) {
        free(config_manager);
        return NULL;
    }

    err = pthread_rwlock_init(&(config_manager->cache_lock), NULL);
    if (err) {
        free(config_manager->work_dir);
        free(config_manager);
        errno = err;
    }

    config_manager->poll_thread = poll_thread;
    config_manager->cache = NULL;

    return config_manager;
}

void config_manager_delete(struct config_manager * config_manager) {
    if (!config_manager) {
        return;
    }

    /* TODO free cache */

    pthread_rwlock_destroy(&(config_manager->cache_lock));
    free(config_manager->work_dir);
    free(config_manager);
}

static struct config_manager_cache * config_manager_cache_find(
        struct config_manager_cache * cache,
        const char * dir_path
) {
    for (; cache; cache = cache->next) {
        if (strcmp(cache->dir_path, dir_path) == 0) {
            break;
        }
    }

    return cache;
}

static void config_rule_delete(struct config_rule * rule) {
    struct config_rule * next = rule;

    while (next) {
        rule = next;
        next = rule->next;

        switch (rule->action) {
        case CONFIG_RULE_ACTION_MIME:
        case CONFIG_RULE_ACTION_CGI:
            free(rule->pattern.extension);
            free(rule->value);
            break;

        case CONFIG_RULE_ACTION_DENY:
            regfree(&(rule->pattern.regexp));
        }

        free(rule);
    }
}

static struct config_manager_cache * config_manager_cache_remove(
        struct config_manager_cache * cache,
        const char * dir_path
) {
    struct config_manager_cache * prev = NULL, * cur;

    for (cur = cache; cur; prev = cur, cur = cur->next) {
        if (strcmp(cur->dir_path, dir_path) == 0) {
            break;
        }
    }

    if (prev) {
        prev->next = cur->next;
    } else {
        cache = cur->next;
    }

    free(cur->dir_path);
    config_rule_delete(cur->rules);
    free(cur);

    return cache;
}

static bool config_rule_resolve(
        struct config_rule * rule,
        const char * basename,
        struct config * result
) {
    bool found = false;

    for (; rule; rule = rule->next) {
        switch (rule->action) {
        case CONFIG_RULE_ACTION_MIME:
            if (is_extension(basename, rule->pattern.extension)) {
                result->action = CONFIG_ACTION_MIME;
                result->value.mime_type = rule->value;
                found = true;
            }

            break;

        case CONFIG_RULE_ACTION_CGI:
            if (is_extension(basename, rule->pattern.extension)) {
                result->action = CONFIG_ACTION_CGI;
                result->value.interpreter = rule->value;
                found = true;
            }

            break;

        case CONFIG_RULE_ACTION_DENY:
            if (regexec(&(rule->pattern.regexp), basename, 0, NULL, 0) == 0) {
                result->action = CONFIG_ACTION_DENY;
                found = true;
            }

            break;
        }
    }

    return found;
}

/*
 * < 0 - error
 * = 0 - done
 * = 1 - more
 * = 2 - invalid
 *
 */
static int config_rule_parse(struct buffer * buffer, struct config_rule ** result) {
    size_t content_length, line_length;
    char * content, * line, * pattern;
    struct config_rule * new_rule;
    int action, err, move = 0;

    if (!buffer || !result) {
        return -EINVAL;
    }

    while (true) {
        content_length = buffer_remaining(buffer);
        if (content_length == 0) {
            return 0;
        }

        content = (char *) buffer_remaining_content(buffer);
        line = strtrim(strnextract(content, content_length, '\n'));

        if (!line) {
            if (buffer_limit(buffer) < buffer_capacity(buffer)) {
                return 1;
            } else {
                return 2;
            }
        }

        line_length = strlen(line);
        move = line_length + 1;

        if (line_length == 0) {
            buffer_move(buffer, move);
            continue;
        }

        if (strncasecmp(line, "mime ", 5) == 0) {
            action = 1;
            memmove(line, line + 5, line_length - 4);
        } else if (strncasecmp(line, "cgi ", 4) == 0) {
            action = 2;
            memmove(line, line + 4, line_length - 3);
        } else if (strncasecmp(line, "deny ", 5) == 0) {
            action = 3;
            memmove(line, line + 5, line_length - 4);
        } else {
            action = 0;
        }

        if (!action) {
            free(line);
            return 2;
        }

        new_rule = malloc(sizeof(struct config_rule));
        if (!new_rule) {
            return -errno;
        }

        new_rule->next = *result;
        new_rule->action = action == 1 ? CONFIG_RULE_ACTION_MIME
                : action == 2 ? CONFIG_RULE_ACTION_CGI
                : CONFIG_RULE_ACTION_DENY;

        pattern = strnextract(line, line_length, ' ');
        if (!pattern) {
            free(new_rule);
            free(line);
            return 2;
        }

        line_length = line_length - strlen(pattern) - 1;
        memmove(line, line + strlen(pattern) + 1, line_length + 1);

        switch (action) {
        case 1:
        case 2:
            new_rule->pattern.extension = pattern;
            break;
        case 3:
            err = regcomp(&(new_rule->pattern.regexp), pattern,
                    REG_EXTENDED | REG_NOSUB | REG_NEWLINE);

            free(pattern);

            if (err) {
                free(new_rule);
                free(line);
                return 2;
            }
        }

        line = realloc(line, sizeof(char) * (line_length + 1));
        new_rule->value = line;
        *result = new_rule;

        buffer_move(buffer, move);
    }
}

static int config_manager_traverse(
        struct config_manager * config_manager,
        char * dir,
        char * basename,
        struct config_manager_handler handler
);

static int config_manager_traverse_handle_parsed_rules(
        struct config_manager_traverse_handler_context * context
) {
    struct config_manager_cache * cache;
    int ret = 0;

    ret = pthread_rwlock_wrlock(&(context->config_manager->cache_lock));
    if (ret) {
        ret = -ret;
        goto free_context;
    }

    cache = config_manager_cache_find(context->config_manager->cache, context->dir);
    if (cache) {
        config_rule_delete(cache->rules);
        cache->rules = context->rules;
    } else {
        cache = malloc(sizeof(struct config_manager_cache));
        if (!cache) {
            ret = -ENOMEM;
            goto free_lock;
        }

        cache->next = context->config_manager->cache;
        cache->dir_path = strdup(context->dir);
        cache->update = time(NULL);
        cache->rules = context->rules;

        context->config_manager->cache = cache;
    }

    context->rules = NULL;

    ret = pthread_rwlock_unlock(&(context->config_manager->cache_lock));
    if (ret) {
        ret = -ret;
        goto free_context;
    }

    ret = config_manager_traverse(
            context->config_manager,
            context->dir,
            context->basename,
            context->handler
    );

    free(context);

    if (ret) {
        goto end;
    }

    goto end;

free_lock:
    pthread_rwlock_unlock(&(context->config_manager->cache_lock));

free_context:
    free(context->dir);
    free(context->basename);
    config_rule_delete(context->rules);
    free(context);

end:
    return ret;
}

static int config_manager_traverse_handler(
        struct config_manager_traverse_handler_context * context,
        struct poll_thread_event event
) {
    int ret;

    ret = buffer_read_fd(context->buffer, event.fd);
    if (ret < 0) {
        return ret;
    } else if (ret == 0) {
        ret = poll_thread_unregister(context->config_manager->poll_thread, event.descriptor);

        if (ret) {
            goto free_context;
        }

        ret = close(event.fd);
        if (ret) {
            ret = -errno;
            goto end;
        }

        buffer_delete(context->buffer);
        context->buffer = NULL;

        ret = config_manager_traverse_handle_parsed_rules(context);
        goto end;
    }

    ret = 0;

    buffer_flip(context->buffer);
    ret = config_rule_parse(context->buffer, &(context->rules));

    switch (ret) {
    case 0:
    case 1:
        buffer_drop_start(context->buffer);

        ret = poll_thread_continue(context->config_manager->poll_thread, event.descriptor);
        if (ret) {
            goto free_poll_thread_register;
        }

        break;

    case 2:
        fprintf(stderr, "[%d] [WARN] Invalid configuration", event.fd);
        ret = poll_thread_unregister(context->config_manager->poll_thread, event.descriptor);
        goto free_context;

    default:
        goto free_poll_thread_register;
    }

    goto end;

free_poll_thread_register:
    poll_thread_unregister(context->config_manager->poll_thread, event.descriptor);

free_context:
    free(context->dir);
    free(context->basename);
    buffer_delete(context->buffer);
    config_rule_delete(context->rules);
    free(context);

/* free_fd: */
    close(event.fd);

end:
    if (ret) {
        errno = -ret;
        fprintf(stderr, "[%d] [WARN] ", event.fd);
        perror("Error while processing config file");
        ret = 0;
    }

    return ret;
}

static int config_manager_traverse(
        struct config_manager * config_manager,
        char * dir,
        char * basename,
        struct config_manager_handler handler
) {
    struct config_manager_traverse_handler_context * handler_context;
    struct config_manager_cache * cache;
    size_t dir_length = strlen(dir);
    struct stat conf_stat;
    struct config result;
    int conf_fd, ret = 0;
    bool resolved;

    if (!strstartswith(dir, config_manager->work_dir)) {
        free(dir);
        free(basename);

        return handler.handler(handler.context, default_config);
    }

    dir[dir_length] = '/';
    strcpy(dir + dir_length + 1, config_filename);

    conf_fd = open(dir, O_NONBLOCK, O_RDONLY);
    if (conf_fd < 0 && errno != ENOENT) {
        ret = -errno;
        goto end;
    }

    dir[dir_length] = '\0';

    if (conf_fd >= 0) {
        /* config file exists */

        ret = fstat(conf_fd, &conf_stat);
        if (ret) {
            ret = -errno;
            goto free_conf_fd;
        }

        ret = pthread_rwlock_rdlock(&(config_manager->cache_lock));
        if (ret) {
            ret = -ret;
            goto free_conf_fd;
        }

        cache = config_manager_cache_find(config_manager->cache, dir);
        if (cache && cache->update >= conf_stat.st_mtime) {
            /* cache for file is actual */

            ret = close(conf_fd);
            if (ret) {
                ret = -errno;

                pthread_rwlock_unlock(&(config_manager->cache_lock));
                goto end;
            }

            resolved = config_rule_resolve(cache->rules, basename, &result);

            ret = pthread_rwlock_unlock(&(config_manager->cache_lock));
            if (ret) {
                ret = -ret;
                goto end;
            }

            if (!resolved) {
                /* there isn't rules for file */

                /* I hope '/' is there */
                *strrchr(dir, '/') = '\0';

                ret = config_manager_traverse(config_manager, dir, basename, handler);
                goto end;
            }

            /* rule for file found */

            free(dir);
            free(basename);
            ret = handler.handler(handler.context, result);

            goto end;
        }

        /* file is not in cache or cache is old */

        ret = pthread_rwlock_unlock(&(config_manager->cache_lock));
        if (ret) {
            ret = -ret;
            goto free_conf_fd;
        }

        handler_context = malloc(sizeof(struct config_manager_traverse_handler_context));
        if (!handler_context) {
            ret = -ENOMEM;
            goto free_conf_fd;
        }

        handler_context->buffer = buffer_new(1024);
        if (!handler_context->buffer) {
            ret = -errno;

            free(handler_context);
            goto free_conf_fd;
        }

        handler_context->config_manager = config_manager;

        handler_context->dir = dir;
        handler_context->basename = basename;
        handler_context->handler = handler;

        handler_context->rules = NULL;

        ret = poll_thread_register(config_manager->poll_thread, conf_fd, POLLIN,
                poll_thread_handler(handler_context, config_manager_traverse_handler));
        if (ret < 0) {
            buffer_delete(handler_context->buffer);
            free(handler_context);
            goto free_conf_fd;
        }

        ret = 0;
        goto end;
    }

    /* config file is not exists */

    ret = close(conf_fd);
    if (ret) {
        ret = -errno;

        pthread_rwlock_unlock(&(config_manager->cache_lock));
        goto end;
    }

    ret = pthread_rwlock_unlock(&(config_manager->cache_lock));
    if (ret) {
        ret = -ret;
        goto end;
    }

    ret = pthread_rwlock_wrlock(&(config_manager->cache_lock));
    if (ret) {
        ret = -ret;
        goto end;
    }

    config_manager->cache = config_manager_cache_remove(config_manager->cache, dir);

    ret = pthread_rwlock_unlock(&(config_manager->cache_lock));
    if (ret) {
        ret = -ret;
        goto end;
    }

    /* I hope '/' is there */
    *strrchr(dir, '/') = '\0';

    ret = config_manager_traverse(config_manager, dir, basename, handler);
    goto end;

/*
free_lock:
    pthread_rwlock_unlock(&(config_manager->cache_lock));
*/

free_conf_fd:
    close(conf_fd);

/* free_dir_basename: */
    free(dir);
    free(basename);

end:
    return ret;
}

int config_manager_resolve(
        struct config_manager * config_manager,
        const char * filename,
        struct config_manager_handler handler
) {
    char * dir, * buf, * basename;

    buf = get_directory(filename);
    if (!buf) {
        return -errno;
    }

    dir = realloc(buf, strlen(buf) + strlen(config_filename) + 2);
    if (!dir) {
        free(buf);
        return -errno;
    }

    basename = get_basename(filename);
    if (!basename) {
        free(dir);
        return -errno;
    }

    return config_manager_traverse(config_manager, dir, basename, handler);

    /*
     *
     * config_manager_resolve(filename, handler) {
     *   for (dir in dirs) {
     *     if (exists(dir + '/' + config_filename)) {
     *       if (!cache_exists(dir)) {
     *         cache_add(dir, parse(dir + '/' + config_filename))
     *       } else if (cache[dir].update < file_mtime(dir + '/' + config_filename)) {
     *         cache_update(dir, parse(dir + '/' + config_filename))
     *       }
     *
     *       if (cache_find(dir, filename)) {
     *         handler(cache[dir][filename])
     *         return
     *       }
     *     } else {
     *       cache_remove(dir)
     *     }
     *   }
     *
     *   handler(default_config)
     * }
     *
     * Для каждой директории от директории указанного файла до рабочей директории:
     * - Если конфигурационный файл существует:
     *   - Если запись в кеше не создана:
     *     - Прочитать конфигурационный файл и создать запись в кеше
     *   - Иначе, если запись в кеше более старая чем конфигурационный файл:
     *     - Прочитать конфигурационный файл и обновить запись в кеше
     *   - Если в записи файла в кеше есть соответствие для указанного файла:
     *     - Вызвать обработчик, передав ему конфигурацию
     *     - Выйти из функции
     * - Иначе:
     *   - Удалить запись о файле из кеша
     * Вызвать обработчик, передав ему конфигурацию по-умолчанию
     *
     */
}
