TARGETS = isprime

CC = clang
CFLAGS = -Wall -Werror -g -I/opt/local/include
LDFLAGS = -L/opt/local/lib -lgmp

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

all: test-isprime $(TARGETS)

isprime: isprime.o
	$(CC) -o $@ $(CFLAGS) $^ $(LDFLAGS)

test-isprime: isprime Makefile
	-./isprime -c 21 23 25 27 29
	-./isprime -p 21 23 25 27 29
	-./isprime -c 12570912837509185701923523092517 12570912837509185701923523092527
	-./isprime -p 12570912837509185701923523092517 12570912837509185701923523092527

.PHONY: clean
clean:
	rm -f *.o *~ $(TARGETS)
