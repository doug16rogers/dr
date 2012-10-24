TARGET = hexdate

CFLAGS = -Wall -Werror -g
CC = gcc

OBJS = hexon.o daysec.o
LIBS = -lm

$(TARGET): $(TARGET).o $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

.PHONY:	xhexclock

clean:
	rm -f core *.o *~ $(TARGET) xhexclock

x:	xhexclock

xhexclock:
	make -f Makefile.xhexclock
