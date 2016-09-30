TARGETS = nonul nocr nobin

all: $(TARGETS)

nonul: nonul.c
	gcc -Wall -Werror -o nonul nonul.c

nocr: nocr.c
	gcc -Wall -Werror -o nocr nocr.c

nobin: nobin.c
	gcc -Wall -Werror -o nobin nobin.c

.PHONY: clean
clean:
	rm -f *~ *.o $(TARGETS)
