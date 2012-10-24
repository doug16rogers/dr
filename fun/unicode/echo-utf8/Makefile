# Copyright (c) Doug Rogers. ALL RIGHTS RESERVED.

TARGET = echo-utf8

CC = gcc
CFLAGS = -g -Wall -Werror

all: $(TARGET)

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

$(TARGET): $(TARGET).o
	$(CC) -o $@ $^

test: $(TARGET)
	$(TARGET) 59 6f 75 20 68 6f 6c 64 20 41 2660 20 31 30 2661 2e
