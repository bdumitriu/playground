% diff(L1,L2,R) - result R is the difference of lists L1 and L2.

member(X,[X|_]).
member(X,[_|T]):-member(X,T).

append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).

diff([],_,[]).
diff([H|T],L,R):-member(H,L),diff(T,L,R).
diff([H|T],L,[H|R]):-\+(member(H,L)),diff(T,L,R).