GHC = ghc
RM = rm -f

PROGRAM = program
SOURCES = \
	SimpleJSON.hs \
	Prettify.hs \
	PrettyJSON.hs \
	Main.hs
OBJECTS = $(SOURCES:%.hs=%.o)
HASKELL_INTERMEDIATES = $(SOURCES:%.hs=%.hi)

all: $(OBJECTS)
	$(GHC) -o $(PROGRAM) $(OBJECTS)

%.o: %.hs
	$(GHC) -c -o $@ $<

.PHONY: clean
clean:
	$(RM) $(OBJECTS)
	$(RM) $(HASKELL_INTERMEDIATES)
	$(RM) $(PROGRAM).exe
