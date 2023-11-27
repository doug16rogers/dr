TARGETS = ack ack-1-1 hello testread

all: $(TARGETS)

%: %.hs
	ghc $<

.PHONY: clean
clean:
	rm -f *.o *.hi *~ $(TARGETS)
