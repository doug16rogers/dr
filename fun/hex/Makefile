TARGET = hex
CFLAGS = -g -Wall -Werror

CXX = g++
CC = gcc

$(TARGET) : $(TARGET).cc
	$(CXX) -o $@ $(CFLAGS) $<

.PHONY: clean
clean:
	rm -f *~ *.o $(TARGET)
	rm -rf $(TARGET).dSYM
