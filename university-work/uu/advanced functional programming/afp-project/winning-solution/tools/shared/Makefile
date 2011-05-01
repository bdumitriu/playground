
include ../Makefile.common

all: interface

interface:
	$(GHC) --make Interface.hs

language: Language.hs
	$(GHC) -o language --make Language
	#$(GHC) -o language --make -main-is Language.main Language

clean:
	rm -f *.o *.hi

.PHONY: all clean interface language

