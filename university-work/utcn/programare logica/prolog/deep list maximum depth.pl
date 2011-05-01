% max_depth(L,R) - R will be the maximum depth of L (i.e.
% the level of the deepest list nested in L).

?-use_module(library(lists)).

max(X,Y,X):-X>Y,!.
max(_,Y,Y).

max_depth([],1).
max_depth([H|T],R):- \+(is_list(H)),!,max_depth(T,R).
max_depth([H|T],R):-max_depth(H,R1),max_depth(T,R2),Z1 is R1+1,max(Z1,R2,R).