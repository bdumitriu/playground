% inv(L,R) - result R is list L with its elements backwards.

append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).

inv([],[]).
inv([H|T],R):-inv(T,R1),append(R1,[H],R).