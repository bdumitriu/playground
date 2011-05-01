MYHOME=/users/jprodrig

JAVA=$(MYHOME)/.nix-profile
STRATEGOXT=$(MYHOME)/.nix-profile
DRYAD=$(MYHOME)/.nix-profile

LIBS = -la stratego-lib -la stratego-xtc

STRINCLUDES = \
  -I $(JAVA)/share/java-front \
  -I $(JAVA)/share/sdf/java-front \
  -I $(JAVA)/share/dryad

extract-method: extract-method.str

better-extract-method: better-extract-method.str

% : %.str Makefile $(wildcard *.str)
	strc -i $< -o $@ $(LIBS) $(STRINCLUDES) --xtc-repo $(DRYAD)/share/dryad/XTC

%.ppstr : %.str
	@pp-stratego -i $< -o $@ $(STRINCLUDES)

%.def : JavaExtractMethod.sdf
	pack-sdf -i JavaExtractMethod.sdf -o $@ -Idef $(JAVA)/share/sdf/java-front/Java-15.def

%.tbl : %.def
	sdf2table -i $< -o $@ -m JavaExtractMethod

%.jemtree : %.java NewJava.tbl
	sglri -i $< -p NewJava.tbl -s CompilationUnit | dryad-reclassify-ambnames -o $@
