% intersect(L1,L2,R) - result R is the intersection of lists L1 and L2.

member(X,[X|_]).
member(X,[_|T]):-member(X,T).

intersect(_,[],[]).
intersect(L,[H|T],[H|R]):-member(H,L),intersect(L,T,R).
intersect(L,[H|T],R):-\+(member(H,L)),intersect(L,T,R).