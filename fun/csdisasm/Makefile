# Stream disassembler using Capstone.

TARGET = csdisasm

CC = gcc
CFLAGS = -Wall -Werror -g -I/opt/local/include

all: $(TARGET)

$(TARGET): $(TARGET).o Makefile
	$(CC) $(CFLAGS) -o $@ $< -L/opt/local/lib -lcapstone
#	$(CC) $(CFLAGS) -o $@ $< /opt/local/lib/libcapstone.a   # This works, too, just 12x larger.
	cat meterpreter.hex | ./$(TARGET) -x -arch=x86-32

.PHONY: clean
clean:
	rm -f $(TARGET) *.o *~
