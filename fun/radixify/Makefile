TARGET = radixify

CC = gcc
CFLAGS = -Wall -Werror -g -I/opt/local/include

LDFLAGS = /opt/local/lib/libgmp.a

OBJS = \
	$(TARGET).o \
	str.o

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $^

all: $(TARGET) test

$(TARGET): $(OBJS) Makefile
	$(CC) -o $@ $(OBJS) $(LDFLAGS)

.PHONY: test
test: $(TARGET) Makefile
	./$(TARGET) u d 111111111111 > test.out       ; echo 12 | diff -u - test.out
	./$(TARGET) d u 13 > test.out                 ; echo 1111111111111 | diff -u - test.out
	./$(TARGET) b d 10100110 > test.out           ; echo 166 | diff -u - test.out
	./$(TARGET) d b 214 > test.out                ; echo 11010110 | diff -u - test.out
	./$(TARGET) o d 166 > test.out                ; echo 118 | diff -u - test.out
	./$(TARGET) d o 246 > test.out                ; echo 366 | diff -u - test.out
	./$(TARGET) d x 166 > test.out                ; echo a6 | diff -u - test.out
	./$(TARGET) d X 166 > test.out                ; echo A6 | diff -u - test.out
	./$(TARGET) x d b7 > test.out                 ; echo 183 | diff -u - test.out
	./$(TARGET) X d b9 > test.out                 ; echo 185 | diff -u - test.out
	./$(TARGET) d d 0034 > test.out               ; echo 34 | diff -u - test.out
	./$(TARGET) b X 101110001001 > test.out       ; echo B89 | diff -u - test.out
	./$(TARGET) b x 101110001001 > test.out       ; echo b89 | diff -u - test.out
	./$(TARGET) b o 101101001001 1100 > test.out  ; echo "5511\n14" | diff -u - test.out
# See "Programming on the Ferranti Mark 1",
# http://curation.cs.manchester.ac.uk/computer50/www.computer50.org/mark1/program.html
# Note that the 5-digit binaries on that page are LSB first but we're MSB first below.
	./$(TARGET) b F 1000111011001011001010001100110101001101 > test.out ; echo 'Z"SLZWRF' | diff -u - test.out
	./$(TARGET) agct 0123 CgAtTacg > test.out     ; echo 21033021 | diff -u - test.out
	./$(TARGET) agct x    CgAtTacg > test.out     ; echo 93c9 | diff -u - test.out
	./$(TARGET) x agct 93c9 > test.out            ; echo cgattacg | diff -u - test.out

.PHONY: clean
clean:
	rm -f $(TARGET) *.o *~
