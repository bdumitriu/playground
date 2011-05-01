#!/bin/bash

#
# elim-Seq is added to the pipe because parse-tiger generates a Seq[expr-parse-tree]
# from an (expr), so the Seq has to be removed, because it is not supported by anf.str
#

echo "Test 1: 2*(5+x)*(7+y+2)+x*5*x*(4+y)"
echo "2*(5+x)*(7+y+2)+x*5*x*(4+y)" | parse-tiger | stri elim-Seq.str | stri anf.str
echo

echo "Test 2: x+6*(x+((t*(5*(x+x+x))*t)+6)*y)"
echo "x+6*(x+((t*(5*(x+x+x))*t)+6)*y)" | parse-tiger | stri elim-Seq.str | stri anf.str | stri anf-format.str
echo

echo "Test 3: (x+y+z+10)*(6*(x+y+20))+5*(x+y*(6+3))"
echo "(x+y+z+10)*(6*(x+y+20))+5*(x+y*(6+3))" | parse-tiger | stri elim-Seq.str | stri anf.str | stri anf-format.str
echo
