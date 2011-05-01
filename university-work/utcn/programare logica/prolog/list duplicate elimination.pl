% elim(L,R) - result R is list L without duplicates.

member(X,[X|_]):-!.
member(X,[_|T]):-member(X,T).

elim([],[]).
elim([H|T],[H|R]):- \+(member(H,T)),elim(T,R).
elim([H|T],R):-member(H,T),elim(T,R).