% izograf(Graph1,Graph2,L) - List L will contain mappings
% of nodes from graph 1 to nodes from graph 2 if the two
% are izomorphic and will fail otherwise.
%
% This implementation uses two lists (an in list and an out list).

g1([[a,[b,c,d]],[b,[a,c]],[c,[a,b,d]],[d,[a,c]]]).
g2([[1,[2,4]],[2,[1,3,4]],[3,[2,4]],[4,[1,2,3]]]).

delete(X,[X|T],T).
delete(X,[H|T],[H|R]):-
	delete(X,T,R).

member(X,[X|_]).
member(X,[_|T]):-
	member(X,T).

izograf(G1,G2,L):-
	perm_eq(G1,G2,eq_vec,[],L).

perm_eq([],[],_,L,L).
perm_eq([H1|T1],L2,Eq,Lin,Lout):-
	delete(H2,L2,T2),
	P =.. [Eq,H1,H2,Lin,L],
	call(P),
	perm_eq(T1,T2,Eq,L,Lout).

eq_vec([N1,T1],[N2,T2],Lin,Lout):-
	eq_nod(N1,N2,Lin,L),
	perm_eq(T1,T2,eq_nod,L,Lout).

eq_nod(N1,N2,Lin,Lin):-
	member(map(N1,N2),Lin),
	!.
eq_nod(N1,N2,Lin,[map(N1,N2)|Lin]):-
	\+member(map(N1,_),Lin),
	\+member(map(_,N2),Lin).

eq_nod(N1,N2,L):-
	var(L),
	!,
	L = [map(N1,N2)|_].
eq_nod(N1,N2,[map(N1,N2)|_]):-
	!.
eq_nod(N1,_,[map(N1,_)|T]):-
	!,
	fail.
eq_nod(_,N2,[map(_,N2)|T]):-
	!,
	fail.
eq_nod(N1,N2,[H|T]):-
	eq_nod(N1,N2,T).