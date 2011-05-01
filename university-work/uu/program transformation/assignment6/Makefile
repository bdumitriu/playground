# Prefix of Tiger
TIGER=$(HOME)/.nix-profile
# Eelco: TIGER=/usr/local/pkg/tiger/current 

# Variable for our tests
checktigs = $(wildcard checks/*.tig)

# Entering just 'make' builds our compiler
all : compile

# This compiles the compiler
compile : compile.str
	strc -i $< -o $@ -la stratego-lib -la stratego-xtc \
	--xtc-repo $(TIGER)/share/tiger/XTC -I $(TIGER)/share/tiger-front

# Check builds a diff for every test
check : $(checktigs:.tig=.diff)

# diff target compares the actual outcome to the expected outcome. A
# diff never fails, to allow execution of all the tests without an
# immediate error.
%.diff : %.out %.cout
	@echo "======================================="
	@echo "testing $* ..."
	@if diff $*.out $*.cout; then \
	  echo "pass: check $* passed"; \
        else \
	  echo "fail: check $* failed"; \
	  echo "interpreted program output:"; \
	  echo "--------------------------------------"; \
	  cat $*.out ; \
	  echo "--------------------------------------"; \
	  echo "compiled program output:"; \
	  echo "--------------------------------------"; \
	  cat $*.cout ; \
	  echo "--------------------------------------"; \
        fi

# A .out file is the result of interpreting a Tiger program
%.out : %.tig
	@$(TIGER)/bin/run-tiger -i $< &> $@

# A .cout file is the result of interpreting a 'compiled' Tiger program
%.cout : %.ctig 
	@$(TIGER)/bin/run-tiger -i $< &> $@

%.ctig : %.tig compile
	@./compile -i $< -o $@

# Remove derived files
clean : 
	rm -f checks/*.out checks/*.cout checks/*.ctig
	rm -rf .libs
	rm -f compile compile.c compile.o compile.dep

# Remove emacs backup files
distclean :
	rm -f *~
	rm -f checks/*~
