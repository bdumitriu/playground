% union(L1,L2,R) - result R is the union of lists L1 and L2.

member(X,[X|_]).
member(X,[_|T]):-member(X,T).

append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).

union(L,[],L).
union(L,[H|T],R):-member(H,L),union(L,T,R).
union(L,[H|T],R):-\+(member(H,L)),append(L,[H],X),union(X,T,R).