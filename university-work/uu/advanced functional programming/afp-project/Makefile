DOC_DIR := doc
API_DIR := $(DOC_DIR)/api
COM_DIR := common
COM_SRC := $(wildcard $(COM_DIR)/*.hs)
SIM_SRC := Simulator.hs
VIS_SRC := Visualizer.hs VisualizerTools.hs VisualizerLib.hs
ANT_SRC := Compiler.hs AntBrain.hs HLAntCode.hs HLAntCombinators.hs ExampleHLAntCode.hs
SOURCES := $(COM_SRC) $(SIM_SRC) $(VIS_SRC)

all : vis sim ant doc 

# make simulator with optimizations
sim : $(COM_SRC) $(SIM_SRC)
	ghc -icommon -o sim --make Simulator -O1

# make visualizer with optimizations
vis : $(VIS_SRC) $(SIM_SRC)
	ghc -icommon -o vis --make Visualizer -O1

# make compiler with optimizations
ant : $(COM_SRC) $(ANT_SRC)
	ghc -icommon -o ant --make AntBrain -O1

doc : api pdf
	

api : $(SOURCES)
	haddock -o $(API_DIR) -h ${SOURCES}

pdf : $(DOC_DIR)/part1.tex
	@cd $(DOC_DIR); pdflatex part1.tex

clean:
	@rm -f *.hi *.o ; \
	rm -f $(COM_DIR)/*.hi $(COM_DIR)/*.o ; \
	rm -f sim ; \
	rm -f vis ; \
	rm -f ant ;\
	rm -f $(API_DIR)/*.html ; \
	rm -f $(DOC_DIR)/*.aux $(DOC_DIR)/*.log $(DOC_DIR)/*.pdf ; \
