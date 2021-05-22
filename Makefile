
CC = gcc
LD = gcc
CFLAGS = -std=gnu89 -pedantic-errors -Wall -Werror -g -O0 # -O2
LFLAGS = -pthread

BUILDPATH = build
SOURCES = main.c server.c poll_thread.c thread_pool.c buffer.c # io_utils.c
HEADERS = server.h poll_thread.h thread_pool.h buffer.h io_utils.h
TARGET = lab3

OBJECTS = $(SOURCES:%.c=$(BUILDPATH)/%.o)

.PHONY: all build clean
.SUFFIXES:

all: build

clean:
	@rm -vrf $(BUILDPATH) 2> /dev/null; true
	@rm -v $(TARGET) 2> /dev/null; true

build: $(TARGET)

%.c:

$(OBJECTS): $(BUILDPATH)/%.o : %.c $(HEADERS)
	@mkdir -p $(@D)
	$(CC) -c -o $@ $< $(CFLAGS)

$(TARGET): $(OBJECTS)
	$(LD) -o $@ $^ $(LFLAGS)
