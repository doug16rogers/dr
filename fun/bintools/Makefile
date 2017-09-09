TARGETS = addcr nonul nocr nobin

CC = gcc
CFLAGS = -Wall -Werror -g

all: $(TARGETS)

.PHONY: install
install: $(TARGETS)
	cp $^ $(DR_BIN)/

addcr: addcr.c
	$(CC) -o $@ $(CFLAGS) $^

nonul: nonul.c
	$(CC) -o $@ $(CFLAGS) $^

nocr: nocr.c
	$(CC) -o $@ $(CFLAGS) $^

nobin: nobin.c
	$(CC) -o $@ $(CFLAGS) $^

.PHONY: clean
clean:
	rm -f *~ *.o $(TARGETS)
