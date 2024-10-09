# Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License.
# You are free to do whatever you want with this software. See LICENSE.txt.

TARGETS = wd-match wd-nodup wd-in wd-score wd-shift

CFLAGS = -Wall -Werror -g
CC = gcc

OBJS = wd-util.o
LIBS = -lm

all: test

.PHONY: test
test: $(addsuffix -test,$(TARGETS))

wd-match: wd-match.o Makefile
	$(CC) $(CLFAGS) -o $@ $< $(LIBS)

wd-in: wd-in.o $(OBJS) Makefile
	$(CC) $(CLFAGS) -o $@ $< $(OBJS) $(LIBS)

wd-nodup: wd-nodup.o $(OBJS) Makefile
	$(CC) $(CLFAGS) -o $@ $< $(OBJS) $(LIBS)

wd-scores: wd-score.o $(OBJS) Makefile
	$(CC) $(CLFAGS) -o $@ $< $(OBJS) $(LIBS)

wd-shift: wd-shift.o $(OBJS) Makefile
	$(CC) $(CLFAGS) -o $@ $< $(OBJS) $(LIBS)

.PHONY: wd-match-test
wd-match-test: wd-match Makefile
	echo "Makefile\nwd-match\na\nb\ntestrant\n" | ./$< abcdefgd > test.out ; \
            echo Makefile | diff -u - test.out
	echo "MakEfile\nwd-match\na\nb\ntestrant\n" | ./$< -i abcdefgd > test.out ; \
            echo MakEfile | diff -u - test.out
	echo "Makefile\nwd-match\na\nb\ntestrant\n" | ./$< -s cdefgdab > test.out ; \
            echo Makefile | diff -u - test.out
	echo "Makefile\nwd-match\na\nb\ntestrant\n" | ./$< -a aabcdefg > test.out ; \
            echo Makefile | diff -u - test.out

.PHONY: wd-in-test
wd-in-test: wd-in Makefile
	echo "Never\nYou\nMind" | ./$< iMdn > test.out ; \
            echo Mind | diff -u - test.out
	echo "Never\nYou\nMind" | ./$< Mdn. > test.out ; \
            echo Mind | diff -u - test.out

.PHONY: wd-nodup-test
wd-nodup-test: wd-nodup Makefile
	echo "sloop\nslop\nplop\nOomph" | ./$< > test.out ; \
	   echo "slop\nOomph" | diff -u - test.out
	echo "sloop\nslop\nplop\nOomph" | ./$< -i > test.out ; \
	   echo "slop" | diff -u - test.out
	echo "sloop\nslop\nplop\nOomph" | ./$< -r > test.out ; \
	   echo "sloop\nplop" | diff -u - test.out
	echo "sloop\nslop\nplop\nOomph" | ./$< -ir > test.out ; \
	   echo "sloop\nplop\nOomph" | diff -u - test.out

.PHONY: wd-score-test
wd-score-test: wd-score Makefile
	./$< -s the quick brown fox > test.out; \
		echo "6 the\n20 quick\n10 brown\n13 fox" | diff -u - test.out
	./$< -w the quick brown fox > test.out; \
		echo "5 the\n22 quick\n12 brown\n13 fox" | diff -u - test.out
	./$< -wn the quick brown fox > test.out; \
		echo "5\n22\n12\n13" | diff -u - test.out
	echo "the quick\nbrown\nfox" | ./$< -w > test.out; \
		echo "5 the\n22 quick\n12 brown\n13 fox" | diff -u - test.out

.PHONY: wd-shift-test
wd-shift-test: wd-shift Makefile
	echo     "howdy\nworlds\n\n" | ./$< > test.out; \
	    echo "owdyh\norldsw\n\n" | diff -u - test.out
	echo     "howdy\nworlds" | ./$< -s=1 > test.out; \
	    echo "owdyh\norldsw" | diff -u - test.out
	echo     "howdy\nworlds" | ./$< -s=2 > test.out; \
	    echo "wdyho\nrldswo" | diff -u - test.out
	echo     "howdy\nworlds" | ./$< -s=3 > test.out; \
	    echo "dyhow\nldswor" | diff -u - test.out
	echo     "howdy\nworlds" | ./$< -s=4 > test.out; \
	    echo "yhowd\ndsworl" | diff -u - test.out
	echo     "howdy\nworlds" | ./$< -s=5 > test.out; \
	    echo "howdy\nsworld" | diff -u - test.out
	echo     "howdy\nworlds" | ./$< -s=0 > test.out; \
	    echo "howdy\nworlds" | diff -u - test.out
	echo     "howdy\nworlds" | ./$< -s=17 > test.out; \
	    echo "wdyho\nsworld" | diff -u - test.out
	echo     "howdy\nworlds" | ./$< -s=18 > test.out; \
	    echo "dyhow\nworlds" | diff -u - test.out

.PHONY: install
install: $(TARGETS) wd-scrabble.sh
	cp $^ $(DR_BIN)/

.PHONY:	clean
clean:
	rm -f core *.o *~ test.out $(TARGETS)
