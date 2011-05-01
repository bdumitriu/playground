ghc --make -icommon -O2 -o temp.exe AntBrain
temp.exe > ant.ant
del temp.exe
