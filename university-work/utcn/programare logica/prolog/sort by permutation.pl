% permsort(UL, SL) - sorts the list UL into SL using permutations.

append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).

delete(X,[X|T],T).
delete(X,[H|T],[H|R]):-delete(X,T,R).

ord(X,Y):-X =< Y.

perm([],[]).
perm(L,[X|R]):-delete(X,L,Y),perm(Y,R).

is_sorted([_]):-!.
is_sorted([]):-!.
is_sorted([X,Y|T]):-ord(X,Y),is_sorted([Y|T]).

permsort(L,R):-perm(L,R),is_sorted(R),!.