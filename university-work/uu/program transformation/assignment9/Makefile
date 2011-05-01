TIGER=$(HOME)/.nix-profile

STRINCLUDES = \
  -I $(TIGER)/share/tiger-front \
  -I $(TIGER)/share/sdf/tiger-front

all : Tiger-Profile

% : %.str
	@strc -i $< -o $@ -la stratego-lib -la stratego-xtc $(STRINCLUDES)

%.ppstr : %.str
	@pp-stratego -i $< -o $@ $(STRINCLUDES)

%.prof.tig : %.tig Tiger-Profile
	@$(TIGER)/bin/parse-tiger -i $< \
                | $(TIGER)/bin/Tiger-Desugar \
                | ./Tiger-Profile \
                | $(TIGER)/bin/Tiger-Ensugar \
                | $(TIGER)/bin/pp-tiger
# -o $@

%.prof.tas : %.tig Tiger-Profile
	@$(TIGER)/bin/parse-tiger -i $< \
                | $(TIGER)/bin/Tiger-Desugar \
                | ./Tiger-Profile \
                | pp-aterm

%.run : %.tig
	@run-tiger -i $<
