# Copyright (c) 2012-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

TARGETS = addcr nonul nocr nobin ccount

CC = gcc
CFLAGS = -Wall -Werror -g

all: $(TARGETS)

.PHONY: install
install: $(TARGETS)
	cp $^ $(DR_BIN)/

addcr: addcr.c
	$(CC) -o $@ $(CFLAGS) $^

ccount: ccount.c
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
