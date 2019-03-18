# Copyright (c) 2005-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

TARGET = hexflash

CC = gcc
CFLAGS = -Wall -Werror

$(TARGET): $(TARGET).o
	$(CC) -o $@ $^

.PHONY: clean
clean:
	rm -f *~ *.o $(TARGET)
