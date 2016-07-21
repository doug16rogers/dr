# Stream disassembler using Capstone.

TARGET = csdisasm

CC = gcc
CFLAGS = -Wall -Werror -g -I/opt/local/include

all: $(TARGET)

$(TARGET): $(TARGET).o Makefile
	$(CC) $(CFLAGS) -o $@ $< -L/opt/local/lib -lcapstone
	cat meterpreter.hex | ./$(TARGET) -x -arch=x86

.PHONY: clean
clean:
	rm -f $(TARGET) *.o *~
