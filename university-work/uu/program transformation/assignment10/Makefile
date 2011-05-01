TIGER=/nix/var/nix/profiles/default

STRINCLUDES = \
  -I $(TIGER)/share/tiger-front \
  -I $(TIGER)/share/sdf/tiger-front

all : tiger-propagate

% : %.str Makefile $(wildcard *.str)
	strc --main io-$* -i $< -o $@ -la stratego-lib -la stratego-xtc \
	--xtc-repo $(TIGER)/share/tiger/XTC $(STRINCLUDES)

clean : 
	rm -f checks/*.out checks/*.cout checks/*.ctig
	rm -rf .libs
	rm -f tiger-propagate tiger-propagate.c tiger-propagate.o tiger-propagate.dep
	rm -f *~
	rm -f checks/*~
