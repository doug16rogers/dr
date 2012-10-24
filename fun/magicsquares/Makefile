# Builds all magic square stuff.

CC  = gcc
SED = sed

ifeq ($(shell uname -s),CYGWIN)
BIN_SUFFIX = .exe
endif

OBJS = \
	dms.o \
	dmc.o \
	dmt.o \

CFLAGS = -Wall -Werror -g

DEP_REGEXP = "s^\($*\)\.o[ :]*^$(OBJ_DIR)\1.o $(OBJ_DIR)\1.d : ^g"

%.d: %.c
	$(CC) -M $(CFLAGS:-Werror=) $< | $(SED) $(DEP_REGEXP) > $@

TARGETS = \
	dexalmagic \
	dexalclassify \
	dexalbase \
	dexaldecompose \
	dmcgen \
	dmtgen \
	index_set_test \
	prove_from_set

# If 'clean=1' (which is used for versions of make that do not support
# MAKECMDGOALS), then assume 'clean' as the target.

ifeq ($(clean),)
all: $(TARGETS)
else
all: clean
endif

dexalmagic: dexalmagic.o
	$(CC) -o $@ $(CFLAGS) $^

dexalclassify: dexalclassify.o $(OBJS)
	$(CC) -o $@ $(CFLAGS) $^

dexaldecompose: dexaldecompose.o
	$(CC) -o $@ $(CFLAGS) $^

dexalbase: dexalbase.o
	$(CC) -o $@ $(CFLAGS) $^

dmcgen: dmcgen.o $(OBJS)
	$(CC) -o $@ $(CFLAGS) $^

dmtgen: dmtgen.o $(OBJS)
	$(CC) -o $@ $(CFLAGS) $^

index_set_test: index_set_test.o index_set.o
	$(CC) -o $@ $(CFLAGS) $^
	./$@ 4 6

prove_from_set: prove_from_set.o index_set.o
	$(CC) -o $@ $(CFLAGS) $^
	./$@

clean:
	rm -f *.o *.d *~ $(addsuffix $(BIN_SUFFIX),$(TARGETS))


# With newer versions of make the MAKECMDGOALS variable is set, so it will
# automatically skip including dependencies when running 'make clean'. For
# older versions, use 'make clean=1'.

ifeq ($(clean),)
ifneq ($(MAKECMDGOALS),clean)
SRCS = $(shell ls *.c)
include $(SRCS:.c=.d)
endif
endif
