#=========================================================================
# Title: Makefile 
# About: Makefile for this application.
#  Typically only the TARGET_NAMES need to be defined.
#  You may also specify OBJS if you aren't using the .c and .cc files
#  in your current directory.
#  To build to separate binary directories based on operating system,
#  comment out BINDIR and OBJDIR.  If you do so, you should do a
#  "make depend ; make clean" to start, since the object files may be
#  from a build under a different operating system.
#***********************************************************************
#
# Specify target executables on following line:
#
TARGET_NAMES = drgo

#
# Only non-target object files should be included below.  You can have
# the makefile determine the object files for you automatically (see further
# down in the makefile) by commenting out the "OBJS =" line below.
#
#OBJS =

#
# Various compiler flags:
#
CC = gcc
CFLAGS = -g -Wall
CXXFLAGS = $(CFLAGS) -Werror

#
# Where to put binaries and object files.  Comment out if you want them
# to be placed in a directory based on the operating system.
#
BINDIR = .
OBJDIR = .

#
# For building to various operating systems:
#
ifndef BINDIR
OSTYPE = $(shell uname -s)
OSVER = $(shell uname -r)

ifeq ($(OSTYPE),SunOS)
ifeq ($(patsubst 5.%,,$(OSVER)),5.)
OSTYPE = Solaris
endif
endif

BINDIR = $(OSTYPE)
TARGETS = $(patsubst %,$(BINDIR)/%,$(TARGET_NAMES))
else
TARGETS = $(TARGET_NAMES)
endif

#
# This won't work with "make depend", so it is overridden below.
#
ifndef OBJDIR
OBJDIR = $(BINDIR)
endif

ifndef OBJS
LOCAL_SRCS  = $(wildcard *.c *.cc)
LOCAL_MODULES = $(basename $(LOCAL_SRCS))
LOCAL_NONTARGET_MODULES = $(filter-out $(TARGET_NAMES),$(LOCAL_MODULES))
LOCAL_NONTARGET_OBJS = $(addsuffix .o,$(LOCAL_NONTARGET_MODULES))
# the following doesn't work with "make depend":
#OBJS = $(patsubst %,$(OBJDIR)/%,$(LOCAL_NONTARGET_OBJS))
OBJS = $(LOCAL_NONTARGET_OBJS)
endif

#
# Libraries, (must include the "-l" for standard libs like "-lm"):
#
LIBS = 

#
# Here's the highest-order target, "all", which depends on the
# targets specified above:
#
all: $(TARGETS)

#
# Make the targets depend on their own object files:
#
TARGET_OBJS = $(addsuffix .o,$(TARGET_NAMES))
$(TARGETS) : $(TARGET_OBJS)

#
# Make the targets depend on the utility object files and libraries, then
# link the targets.
#
$(TARGETS): $(OBJS) $(LIBS)
	gcc -g -o $@ $(@F).o $(OBJS) $(LIBS)

# cleaning out object files:

clean:
	rm -f $(OBJS)
	rm -f $(TARGET_OBJS)

# auto-dependency generation...

LOCAL_C_FILES  = $(wildcard *.c)
LOCAL_CC_FILES = $(wildcard *.cc)

depend:
	sed '/\#\#\# Dependencies/q' < Makefile > tmp_make
	for i in $(LOCAL_C_FILES) $(LOCAL_CC_FILES) ; \
	     do $(CPP) -MM $$i;done >> tmp_make
	mv tmp_make Makefile


### Dependencies:
drgo.o: drgo.c gmp.h go_board.h
gmp.o: gmp.c gmp.h
go_board.o: go_board.c go_board.h
pattern.o: pattern.c pattern.h
