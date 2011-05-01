% path(X,Y,L) - looks for a path from vertex X to vertex
% Y in a graph and returns the result in list L.

:-use_module(library(lists)).

n_list(a,[b,c,d]).
n_list(b,[e,f]).
n_list(c,[b,e]).
n_list(d,[e]).
n_list(e,[f]).
n_list(f,[]).
n_list(g,[]).

edge(X,Y):-n_list(X,L),member(Y,L).

path(X,Y,_,[X,Y]):-edge(X,Y).
path(X,Y,Lp,[X|L]):-edge(X,Z),\+member(Z,Lp),path(Z,Y,[Z|Lp],L).

path(X,Y,L):-path(X,Y,[],L).