#include "server.h"


int main() {
    struct server_config server_config = { ".", 8080 };

    return server_main(server_config);
}
