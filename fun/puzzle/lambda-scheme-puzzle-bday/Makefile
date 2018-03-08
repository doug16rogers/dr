TARGETS = fib3 fib3-scanner

CC = clang
CFLAGS = -Wall -Werror -g
LDFLAGS = -g

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

all: $(TARGETS)

fib3: fib3.o
	$(CC) -o $@ $^ $(LDFLAGS)

fib3-scanner: fib3-scanner.o
	$(CC) -o $@ $^ $(LDFLAGS)

.PHONY: run-guile
run-guile: lambda-scheme-puzzle-bday.sch
# So annoying that there's no way to turn off the banner - and it is
# emitted to stdout not stderr.
	guile -q < $<

.PHONY: clean
clean:
	rm -f *~ *.o $(TARGETS)
