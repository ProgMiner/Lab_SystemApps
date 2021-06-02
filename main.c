#include <termios.h>

#include "server.h"


/* https://habr.com/ru/post/124789/ */
struct termios set_keypress() {
	struct termios stored_settings, new_settings;

	tcgetattr(0, &stored_settings);

	new_settings = stored_settings;
	new_settings.c_lflag &= (~ICANON & ~ECHO);
	new_settings.c_cc[VTIME] = 0;
	new_settings.c_cc[VMIN] = 1;
	tcsetattr(0, TCSANOW, &new_settings);

	return stored_settings;
}

void reset_keypress(struct termios stored_settings) {
	tcsetattr(0, TCSANOW, &stored_settings);
}

int main() {
    struct server_config server_config = { ".", 8080 };
    struct termios stored_termios;
    int ret;

    stored_termios = set_keypress();
    ret = server_main(server_config);
    reset_keypress(stored_termios);

    return ret;
}
