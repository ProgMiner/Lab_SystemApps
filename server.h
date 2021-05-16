#pragma once

#include <netinet/in.h>
#include <stdint.h>


struct server_config {
    const char * work_dir;

    uint16_t server_port;
    struct in6_addr server_addr;
};

int server_main(struct server_config config);
