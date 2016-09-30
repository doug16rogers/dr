TARGETS = ascii-chart morse-chart

CC = gcc
CFLAGS = -Wall -Werror -O3

all: $(TARGETS)

ascii-chart: ascii-chart.c Makefile
	$(CC) -o $@ $(CFLAGS) $<

morse-chart: morse-chart.c Makefile
	$(CC) -o $@ $(CFLAGS) $<

.PHONY: clean
clean:
	rm -f *.o *~ $(TARGETS)

.PHONY: check
check: $(TARGETS)
	for tgt in $(TARGETS); do \
		echo ./$$tgt ; \
		./$$tgt ; \
	done
