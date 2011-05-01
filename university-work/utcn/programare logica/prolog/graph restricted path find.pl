% r_path(X,Y,R,L) - looks for a path from vertex X to vertex
% Y in a graph having R as a restriction list and returns the
% result in list L. The restriction list contains the vertices
% that must be part of the path.

:-use_module(library(lists)).

n_list(a,[b,c,d]).
n_list(b,[e,f]).
n_list(c,[b,e]).
n_list(d,[e]).
n_list(e,[f]).
n_list(f,[]).
n_list(g,[]).

edge(X,Y):-n_list(X,L),member(Y,L).

r_path(X,Y,_,[],[X,Y]):-edge(X,Y).
r_path(X,Y,Lp,[H|T],[X|L]):-edge(X,H),\+member(H,Lp),r_path(H,Y,[H|Lp],T,L).
r_path(X,Y,Lp,R,[X|L]):-edge(X,Z),\+member(Z,Lp),r_path(Z,Y,[Z|Lp],R,L).

r_path(X,Y,R,L):-r_path(X,Y,[],R,L).