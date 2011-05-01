
include ../Makefile.common

all:
	$(MAKE) -C ../shared
	$(GHC) -i../shared --make Simulator -o simulator

clean:
	$(MAKE) -C ../shared clean
	rm -f *.o *.hi

.PHONY: all clean

