# Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

# Stream disassembler using Capstone.

TARGET = csdisasm

# OPT_DIR=/usr/local
OPT_DIR=$(HOME)/opt

CC = gcc
CFLAGS = -Wall -Werror -g -I$(OPT_DIR)/include

all: $(TARGET)

.PHONY: install
install: $(TARGET)
	cp $^ $(DR_BIN)/

$(TARGET): $(TARGET).o Makefile
	$(CC) $(CFLAGS) -o $@ $< -L$(OPT_DIR)/lib -lcapstone
#	$(CC) $(CFLAGS) -o $@ $< /opt/local/lib/libcapstone.a   # This works, too, just 12x larger.
#	cat meterpreter.hex | ./$(TARGET) -x -arch=x86-32

.PHONY: clean
clean:
	rm -f $(TARGET) *.o *~
