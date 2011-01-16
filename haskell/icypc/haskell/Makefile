GHC = ghc
RM = rm -f

PROGRAM = program
SOURCES = \
	GameState.hs \
	GameStateParser.hs \
	Main.hs
OBJECTS = $(SOURCES:%.hs=%.o)
HASKELL_INTERMEDIATES = $(SOURCES:%.hs=%.hi)

all:
	$(GHC) --make -o $(PROGRAM) Main

%.o: %.hs
	$(GHC) -c -o $@ $<

.PHONY: clean
clean:
	$(RM) $(OBJECTS)
	$(RM) $(HASKELL_INTERMEDIATES)
	$(RM) $(PROGRAM).exe
