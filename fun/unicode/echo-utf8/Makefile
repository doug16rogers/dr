# Copyright (c) Doug Rogers. ALL RIGHTS RESERVED.

TARGET = echo-utf8

CC = gcc
CFLAGS = -g -Wall -Werror

all: $(TARGET)

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

.PHONY: install
install: $(TARGET)
	cp $^ $(DR_BIN)/

$(TARGET): $(TARGET).o
	$(CC) -o $@ $^

.PHONY: test
test: $(TARGET)
	./$(TARGET) 50 6c 61 79 68 6e 67 20 63 61 72 64 73 3a 20 \
		41 2660 20 4b 2662 20 51 2661 20 4a 2663 20 31 30 2661
	@echo
	./$(TARGET) 43 79 72 75 73 20 74 68 65 20 47 72 65 61 74 20 69 6e 20 63 75 6e 65 69 66 6f 72 6d 3a 20 \
		103a4 103a2 103bd 103c1

.PHONY: clean
clean:
	rm -f $(TARGET) *.o *~

