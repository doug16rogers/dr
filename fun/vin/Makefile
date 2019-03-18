# Copyright (c) 2015-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

# checksum checker for VINs.

TARGET = vinsum

all: $(TARGET) test

$(TARGET): $(TARGET).c vin.c
	clang -o $@ -g -Wall -Werror $^

.PHONY: test
test: $(TARGET) Makefile
# VINs found by searching online.
	./$(TARGET) JH4TB2H26CC000000 1HGES15541L000000 W0L0XCF2143043593 LJCPCBLCX11000237

.PHONY: clean
clean:
	rm -f *.o *~ $(TARGET)
