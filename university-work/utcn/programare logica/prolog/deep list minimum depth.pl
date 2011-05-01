% min_depth(L,R) - R will be the minimum depth of L (i.e.
% the level of the shallowest list nested in L. If L contains
% atomic elements or is [], R will be 1).

?-use_module(library(lists)).

min(X,Y,X):-X<Y,!.
min(_,Y,Y).

min_depth([],1).
min_depth([H|_],1):- \+(is_list(H)),!.
min_depth([H|T],R):-T\==[],!,min_depth(H,R1),Z1 is R1+1,min_depth(T,R2),min(Z1,R2,R).
min_depth([H|_],R):-min_depth(H,R1),R is R1+1.