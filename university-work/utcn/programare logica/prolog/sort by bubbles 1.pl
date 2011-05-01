% bubblesort(UL, SL) - sorts the list UL into SL using bubble sort.

ord(X,Y):-X=<Y.

bubblesort(X,Y,F):-bsort(X,Z,F),ground(F),!,bubblesort(Z,Y,_).
bubblesort(X,X,_).

bsort([X,Y],[X,Y],_):-ord(X,Y),!.
bsort([X,Y],[Y,X],F):-F=0,!.
bsort([X,Y|T],[X|R],F):-ord(X,Y),!,bsort([Y|T],R,F).
bsort([X,Y|T],[Y|R],F):-F=0,bsort([X|T],R,F).