% inv(L,R) - result R is list L with its elements backwards.

inv([],R,R).
inv([H|T],Rp,R):-inv(T,[H|Rp],R).

inv(L,R):-inv(L,[],R).