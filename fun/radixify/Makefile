TARGET = radixify

CC = gcc
CFLAGS = -Wall -Werror -g -I/opt/local/include

LDFLAGS = /opt/local/lib/libgmp.a

OBJS = \
	$(TARGET).o \
	str.o

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $^

all: $(TARGET)

$(TARGET): $(OBJS) Makefile
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

.PHONY: clean
clean:
	rm -f $(TARGET) *.o *~
