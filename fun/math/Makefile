# Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

TARGETS = isprime mksieve all-sieve-primes factor perm

# This will allow process substitution like `diff <(prog arg1 arg2) <(echo expected)`.
SHELL = /bin/bash

CC = gcc
CFLAGS = -Wall -Werror -g -I/opt/local/include
LDFLAGS = -L/opt/local/lib -lgmp

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

all: $(TARGETS) test-factor test-isprime test-mksieve test-perm

factor: factor.o sieve.o sieve-1K.o
	$(CC) -o $@ $(CFLAGS) $^ $(LDFLAGS)

.PHONY: test-factor
test-factor: factor Makefile
	diff -u <(./factor 0) <(echo "0")
	diff -u <(./factor 1) <(echo "1")
	diff -u <(./factor 2) <(echo "2")
	diff -u <(./factor 3) <(echo "3")
	diff -u <(./factor 4) <(echo "2 2")
	diff -u <(./factor 5) <(echo "5")
	diff -u <(./factor 6) <(echo "2 3")
	diff -u <(./factor 7) <(echo "7")
	diff -u <(./factor 8) <(echo "2 2 2")
	diff -u <(./factor 9) <(echo "3 3")
	diff -u <(./factor 10) <(echo "2 5")
	diff -u <(./factor 21) <(echo "3 7")
	diff -u <(./factor 111) <(echo "3 37")
	diff -u <(./factor 2442) <(echo "2 3 11 37")
	diff -u <(./factor 1600) <(echo "2 2 2 2 2 2 5 5")

isprime: isprime.o
	$(CC) -o $@ $(CFLAGS) $^ $(LDFLAGS)

.PHONY: test-isprime
test-isprime: isprime Makefile
	diff -u <(./isprime -c 21 23 25 27 29) <(printf "21\n25\n27\n")
	diff -u <(./isprime -p 21 23 25 27 29) <(printf "23\n29\n")
	diff -u <(./isprime -c 12570912837509185701923523092517 12570912837509185701923523092527) \
		<(echo 12570912837509185701923523092517)
	diff -u <(./isprime -p 12570912837509185701923523092517 12570912837509185701923523092527) \
		<(echo 12570912837509185701923523092527)

mksieve: mksieve.o
	$(CC) -o $@ $(CFLAGS) $^ $(LDFLAGS)

all-sieve-primes: all-sieve-primes.c sieve-1G.c
	$(CC) -o all-sieve-primes $(CFLAGS) all-sieve-primes.c sieve.c sieve-1K.c

sieve-1K.c: mksieve
	./mksieve 1024 > sieve-1K.c

sieve-1G.c: mksieve
	./mksieve 1048576 > sieve-1G.c

sieve-4G.c: mksieve
	@echo '* Building 64-bit sieve. This might take a minute or so...'
	./$^ 4294967296 > $@

.PHONY: test-mksieve
test-mksieve: mksieve all-sieve-primes Makefile
	./all-sieve-primes > all-sieve-primes.txt
	diff primes-to-1024.txt all-sieve-primes.txt

perm: perm.c
	$(CC) -o perm $(CFLAGS) $^

.PHONY: test-perm
test-perm: perm Makefile
	./perm 0123 | diff - perm-0123.txt
	./perm 012345 | diff - perm-012345.txt

.PHONY: install
install:
	cp -f isprime factor perm $(DR_BIN)

.PHONY: clean
clean:
	rm -f *.o *~ $(TARGETS) all-sieve-primes sieve-*.c
