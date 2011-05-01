% o_path(X,Y,L) - looks for the shortest (optimal) path from 
% vertex X to vertex Y in a graph and returns the result in 
% list L.

:-use_module(library(lists)).

n_list(a,[b,c,d]).
n_list(b,[e,f]).
n_list(c,[b,e]).
n_list(d,[e]).
n_list(e,[f]).
n_list(f,[]).
n_list(g,[]).

edge(X,Y):-n_list(X,L),member(Y,L).

o_path_(X,Y,Lp,Length):-
	edge(X,Y),
	L1 is Length+1,
	retract(o_path_so_far(_,_)),
	assert(o_path_so_far([Y|Lp],L1)).
o_path_(X,Y,Lp,Length):-
	edge(X,Z),
	\+member(Z,Lp),
	o_path_so_far(_,Lo),
	L1 is Length+1,
	L1<Lo,
	o_path_(Z,Y,[Z|Lp],L1).

o_path(X,Y,_,_):-
	assert(o_path_so_far([],500)),
	o_path_(X,Y,[X],0),
	fail.
o_path(_,_,L,Length):-
	retract(o_path_so_far(Lrev,Length)),
	reverse(Lrev,L).