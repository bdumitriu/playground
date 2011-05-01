% izograf(Graph1,Graph2,L) - List L will contain mappings
% of nodes from graph 1 to nodes from graph 2 if the two
% are izomorphic and will fail otherwise.
%
% This implementation uses side effects.

:-dynamic izo/2.

g1([[a,[b,c,d]],[b,[a,c]],[c,[a,b,d]],[d,[a,c]]]).
g2([[1,[2,4]],[2,[1,3,4]],[3,[2,4]],[4,[1,2,3]]]).

izograf(G1,G2,L):-
	delete_izo,
	perm_eq(G1,G2,eq_vec),
	findall((X,Y),izo(X,Y),L),
	!.

delete_izo:-
	retract(izo(_,_)),
	fail.
delete_izo.

eq_vec([N1,L1],[N2,L2]):-
	eq_nod(N1,N2),
	perm_eq(L1,L2,eq_nod).

eq_nod(N1,N2):-
	izo(N1,N2),
	!.
eq_nod(N1,_):-
	izo(N1,_),
	!,
	fail.
eq_nod(_,N2):-
	izo(_,N2),
	!,
	fail.
eq_nod(N1,N2):-
	asserta(izo(N1,N2)).
eq_nod(N1,N2):-
	retract(izo(N1,N2)),
	!,
	fail.

perm_eq([],[],_).
perm_eq([H1|T1],L2,Eq):-
	delete(H2,L2,T2),
	P =.. [Eq,H1,H2],
	call(P),
	perm_eq(T1,T2,Eq).

delete(X,[X|T],T).
delete(X,[H|T],[H|R]):-
	delete(X,T,R).