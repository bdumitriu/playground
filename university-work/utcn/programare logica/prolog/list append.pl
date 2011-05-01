% append(L1,L2,R) - appends list L2 to list L1 in result R.

append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).