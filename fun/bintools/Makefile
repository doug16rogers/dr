# Copyright (c) 2012-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

TARGETS = addcr nonul nocr nobin ccount b2x x2b

CC = gcc
CFLAGS = -Wall -Werror -g

all: $(TARGETS)

.PHONY: install
install: $(TARGETS)
	cp $^ $(DR_BIN)/

%: %.c
	$(CC) -o $@ $(CFLAGS) $<

.PHONY: clean
clean:
	rm -f *~ *.o $(TARGETS)
