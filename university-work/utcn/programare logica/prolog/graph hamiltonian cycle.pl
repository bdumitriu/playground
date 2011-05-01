% ham(X,R) - R will be a hamiltonian cycle starting
% and ending with X. Fails if no hamiltonian cycle
% exists.

:-dynamic(parc/1).
:-dynamic(drum_optim/2).
:-dynamic(nod/1).

arc_o(a,b).
arc_o(a,h).
arc_o(b,a).
arc_o(b,c).
arc_o(b,f).
arc_o(b,g).
arc_o(c,d).
arc_o(d,b).
arc_o(d,c).
arc_o(e,d).
arc_o(e,f).
arc_o(e,g).
arc_o(f,a).
arc_o(f,c).
arc_o(f,d).
arc_o(f,e).
arc_o(f,g).
arc_o(g,a).
arc_o(g,h).
arc_o(h,b).
arc_o(h,c).
arc_o(h,f).

reverse([],L,L).
reverse([H|T],U,V):-
	reverse(T,U,[H|V]).

member(X,[X|_]).
member(X,[_|T]):-
	member(X,T).

ham(1,X,Y,L,L):-X\==Y,arc_o(Y,X).
ham(N,X,Y,Lparc,R):-
	arc_o(Y,Z),
	\+member(Z,Lparc),
	N1 is N-1,
	ham(N1,X,Z,[Z|Lparc],R).

inc:-
	retract(nr_noduri(N)),
	!,
	N1 is N+1,
	asserta(nr_noduri(N1)).

getnext:-
	retract(nod(X)),
	!,
	X\==end.

collect:-
	getnext,
	inc,
	collect.
collect.

trav:-
	arc_o(X,_),
	\+nod(X),
	asserta(nod(X)),
	fail.
trav.

ham(X,R):-
	asserta(nod(end)),
	asserta(nr_noduri(0)),
	trav,
	collect,
	retract(nr_noduri(N)),
	ham(N,X,X,[X],Ri),
	reverse([X|Ri],R,[]).