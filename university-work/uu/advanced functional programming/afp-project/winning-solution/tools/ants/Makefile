include ../Makefile.common

ANTS = FoodFetch StartAntTest StartAntTest2 GaneshTest DoNothing BaseFind

DEPS = ../compiler/Library.hs ../shared/Language.hs ../shared/Marks.hs \
       Paths.hs Walk.hs Markers.hs StartAnt.hs Tune.hs

all: $(ANTS) $(ANTS:=.ant)

GTT = _turnprob2randomrep3 _turnprob7rep4 _turnprob8rep4 _turnprob6rep4 \
      _turnprob5rep4 _turnprob4rep4 _wrfindfoodafter0 _wrfindfoodafter1 \
      _wrfindfoodafter2 _escapenum20 _escapenum50 _escapenum100 \
      _gohomeanywayprob500 _gohomeanywayprob1000 _gohomeanywayprob2000 \
      _dontvisitfoehome0 _leavebait0 _dontvisitfoehome0leavebait0

$(subst _,GaneshTest_,$(GTT)): %: GaneshTest.hs $(DEPS)
	rm Tune.o || true
	$(GHC) -cpp -DTUNEDATA='"$(subst GaneshTest_,,$@)"' \
	   -i../shared -i../compiler -o $@ \
	       --make -main-is GaneshTest.main GaneshTest

$(ANTS): %: %.hs ../shared/Marks.hs $(DEPS)
	rm Tune.o || true
	$(GHC) -cpp -DTUNEDATA='""' -i../shared -i../compiler -o $@ \
	   --make -main-is $@.main $@

%.ant: %
	./$< > $@ || (rm $@ && exit 1)

clean:
	rm -f *.o *.hi $(ANTS:=.ant) $(ANTS)

.PHONY: all clean

