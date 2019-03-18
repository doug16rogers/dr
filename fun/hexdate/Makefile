# Copyright (c) 2010-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

TARGET = hexdate

CFLAGS = -Wall -Werror -g
CC = gcc

OBJS = daysec.o ezlog.o hexon.o
LIBS = -lm

all: $(TARGET)

.PHONY: install
install: $(TARGET)
	cp $^ $(DR_BIN)/

$(TARGET): $(TARGET).o $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

.PHONY:	clean
clean:
	rm -f core *.o *~ $(TARGET) xhexclock

x:	xhexclock

xhexclock:
	make -f Makefile.xhexclock
