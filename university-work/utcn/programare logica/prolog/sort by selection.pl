% selsort(UL, SL) - sorts the list UL into SL using selection sort.

ord(X,Y):-X=<Y.

min([H|T],Min,[H|R]):-min(T,Min,R),ord(Min,H),!.
min([H|T],H,T).

selsort([],[]).
selsort(L,[Min|R]):-min(L,Min,Y),selsort(Y,R).
