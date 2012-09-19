TARGET = hexpaper

CC = gcc
CFLAGS = -Wall -Werror

$(TARGET): $(TARGET).o
	$(CC) -o $@ $< $(OBJS)

try: $(TARGET)
	./$(TARGET) > default.ps && evince default.ps

clean:
	rm -f $(TARGET) *.o *~ *.BAK *.bak core

