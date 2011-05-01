------------------------------------------
AFP Project 2005
by Bogdan Dumitriu,
   José Pedro Magalhães and
   Huanwen Qu
------------------------------------------
   
This project consists of 3 different parts.
The simulator can be compiled with: 
  ghc -icommon -O2 --make -o simulator.exe -package wx Simulator

The visualizer can be compiled with:
  ghc -icommon -package wx -O2 --make -o visualizer.exe Visualizer
  
The compiler is used on a different fashion. Haskell files that
want to use our high level ant language should include the files
Compiler, HLAntCombinators, HLAntCode and possibly ExampleHLAntCode.
Then, defining a main function like:
  main = outputCompile $ strategy
will generate an executable that will output the low level code resulting
from the high level code in 'strategy'. This is clearly shown on 
file AntBrain.hs, which is our own ant. The resulting ant file can 
be obtained by:
  ghc --make -icommon -O2 -o temp.exe AntBrain
  temp.exe > ant.ant
  del temp.exe

Note that we also supply three .bat files for this purpose in Windows, and
a Makefile with all the general utilities for a Unix system.

Both the simulator and the visualizer have the following usage:
  Usage: <program-name> -w worldFile -r redAntFile [-b blackAntFile] 
    [-c numberOfRounds][-s randomSeed] [-o]
  
    -w FILE  --world=FILE       the world file to use
    -r FILE  --red-ant=FILE     the ant code file for red ants
    -b FILE  --black-ant=FILE   the ant code file for black ants (optional)
    -c INT   --nr-rounds=INT    the number of rounds to simulate (optional)
    -s INT   --random-seed=INT  the initial seed for the RNG (optional)
    -o       --output           use if you want to dump output (optional)

A typical execution could be:
visualizer -w worlds\sample0.world -r ants\sample.ant -b ants\sample.ant

Both the visualizer and the simulator have had minimal changes since
their initial submission.

The code has been tested with GHC versions 6.2.2 and 6.4,
but GHCi only version 6.2.2, because of wxHaskell.

A paper about the simulator can be found on the file
doc/part1.tex.
A brief, manual-like explanation of the visualizer can be 
found on the file doc/visualizerManual.html.
The slides of the presentation about the compiler and the
ant are on doc/presentation.tex file.

A more technical explanation can be obtained by generating the 
haddock documention of the modules (run "make api"). Note that,
since there are several Main modules, one might want to generate
documentation for smaller subsets of the source files instead.
Even more documentation can be found in the source code itself, 
in the form of comments, namely explaining the behaviour of the ant.