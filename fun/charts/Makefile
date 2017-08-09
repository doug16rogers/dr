TARGETS = ascii-chart morse-chart element-chart

CC = gcc
CFLAGS = -Wall -Werror -O2 -g

all: $(TARGETS)

ascii-chart: ascii-chart.c Makefile
	$(CC) -o $@ $(CFLAGS) $<

morse-chart: morse-chart.c Makefile
	$(CC) -o $@ $(CFLAGS) $<

element-chart: element-chart.c text_canvas.c Makefile
	$(CC) -o $@ $(CFLAGS) $< text_canvas.c

unit_test: unit_test.c text_canvas_test.c text_canvas.c cut.c
	$(CC) -o $@ $(CFLAGS) $^

.PHONY: clean
clean:
	rm -f *.o *~ $(TARGETS)

.PHONY: check
check: $(TARGETS)
	for tgt in $(TARGETS); do \
		echo ./$$tgt ; \
		./$$tgt ; \
	done
