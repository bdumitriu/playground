% inv(L,R) - result R is list L with its elements backwards.

inv([],R,R).
inv([H|T],P,R):-inv(T,[H|P],R).

inv(L,R):-inv(L,[],R).