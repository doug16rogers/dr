# Copyright (c) 2010-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

TARGET = hexpaper

CC = gcc
CFLAGS = -Wall -Werror

$(TARGET): $(TARGET).o
	$(CC) -o $@ $< $(OBJS)

try: $(TARGET)
	./$(TARGET) > default.ps && evince default.ps

clean:
	rm -f $(TARGET) *.o *~ *.BAK *.bak core

