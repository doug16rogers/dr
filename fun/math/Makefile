TARGETS = isprime mksieve all-sieve-primes factor perm

CC = clang
CFLAGS = -Wall -Werror -g -I/opt/local/include
LDFLAGS = -L/opt/local/lib -lgmp

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

all: $(TARGETS) test-isprime test-mksieve test-perm

isprime: isprime.o
	$(CC) -o $@ $(CFLAGS) $^ $(LDFLAGS)

mksieve: mksieve.o
	$(CC) -o $@ $(CFLAGS) $^ $(LDFLAGS)

factor: factor.o sieve.o sieve-4G.o
	$(CC) -o $@ $(CFLAGS) $^ $(LDFLAGS)

.PHONY: test-mksieve
test-mksieve: mksieve all-sieve-primes Makefile
	./all-sieve-primes > all-sieve-primes.txt
	diff primes-to-1024.txt all-sieve-primes.txt

all-sieve-primes: all-sieve-primes.c sieve-1K.c
	$(CC) -o all-sieve-primes $(CFLAGS) all-sieve-primes.c sieve.c sieve-1K.c

sieve-1K.c: mksieve
	./mksieve 1024 > sieve-1K.c

sieve-4G.c: mksieve
	@echo '* Building 64-bit sieve. This might take a minute or so...'
	./$^ 4294967296 > $@

.PHONY: test-isprime
test-isprime: isprime Makefile
	./isprime -c 21 23 25 27 29 > test.out; printf "21\n25\n27\n" | diff - test.out
	./isprime -p 21 23 25 27 29 > test.out; printf "23\n29\n" | diff - test.out
	./isprime -c 12570912837509185701923523092517 12570912837509185701923523092527 > test.out; \
		echo 12570912837509185701923523092517 | diff - test.out
	./isprime -p 12570912837509185701923523092517 12570912837509185701923523092527 > test.out; \
		echo 12570912837509185701923523092527 | diff - test.out

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
	rm -f *.o *~ $(TARGETS) all-sieve-primes sieve-4G.c sieve-1K.c
