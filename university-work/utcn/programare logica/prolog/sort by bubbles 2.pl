% bubblesort(UL, SL) - sorts the list UL into SL using bubble sort.

ord(X,Y):-X=<Y.

append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).

bubblesort(L,S):-append(X,[A,B|Y],L),ord(B,A),append(X,[B,A|Y],M),!,bubblesort(M,S).
bubblesort(L,L).
