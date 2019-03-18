# Copyright (c) 2003-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

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
