TARGETS = hex
CFLAGS = -g -Wall -Werror

CXX = g++
CC = gcc

all: $(TARGETS)

.PHONY: install
install: $(TARGETS)
	cp $^ $(DR_BIN)/

$(TARGETS) : $(TARGETS).cc
	$(CXX) -o $@ $(CFLAGS) $<

.PHONY: clean
clean:
	rm -f *~ *.o $(TARGETS)
	rm -rf $(TARGETS).dSYM
