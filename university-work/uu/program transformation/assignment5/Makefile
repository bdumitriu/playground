# Variable for our tests
checktigs = $(wildcard checks/*.tig)

# Entering just 'make' builds our transformation tool
all : factorout	

# This compiles the factorout transformation tool
factorout : factorout.str
	strc -i $< -O 2 -Cl stratego-lib

# Check builds a diff for every test
check : $(checktigs:.tig=.diff)

# Remove derived files
clean :
	rm -f checks/*.fou checks/*.af
	rm -rf .libs
	rm -f factorout factorout.c factorout.o factorout.dep

# Remove emacs backup files
distclean :
	rm -f *~
	rm -f checks/*~

# diff target compares the actual outcome to the expected outcome. A
# diff never fails, to allow execution of all the tests without an
# immediate error.
%.diff : %.fou
	@if test -f $*.exp; then \
	  if diff $*.fou $*.exp; then \
	    echo "pass: check $* passed"; \
          else \
	    echo "fail: check $* failed"; \
          fi \
	else \
	  echo "warning: no expected outcome specified for $*"; \
	fi

# A.fou file is the result of a factorout application
%.fou : %.tig factorout
	@parse-tiger -i $< \
        | ./factorout --factor x --verbose 2 \
        | format-check --rtg Subset.rtg \
        | Tiger-Ensugar \
        | pp-tiger -o $@

# Show AST after factorout application
%.af : %.tig factorout
	parse-tiger -i $< | ./factorout --factor y | pp-aterm -o $@
