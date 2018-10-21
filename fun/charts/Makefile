TARGETS = ascii-chart morse-chart element-chart

CC = gcc
CFLAGS = -Wall -Werror -O2 -g

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

all: $(TARGETS)

.PHONY: install
install: $(TARGETS)
	cp $^ $(DR_BIN)/

ascii-chart: ascii-chart.c Makefile
	$(CC) -o $@ $(CFLAGS) $<

morse-chart: morse-chart.o text_canvas.o
	$(CC) -o $@ $(CFLAGS) $^

element-chart: element-chart.o text_canvas.o
	$(CC) -o $@ $(CFLAGS) $^

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
