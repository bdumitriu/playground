% izograf(Graph1,Graph2,L) - List L will contain mappings
% of nodes from graph 1 to nodes from graph 2 if the two
% are izomorphic and will fail otherwise.
%
% This implementation uses one list ended in a variable.

g1([[a,[b,c,d]],[b,[a,c]],[c,[a,b,d]],[d,[a,c]]]).
g2([[1,[2,4]],[2,[1,3,4]],[3,[2,4]],[4,[1,2,3]]]).

delete(X,[X|T],T).
delete(X,[H|T],[H|R]):-
	delete(X,T,R).

member(X,[X|_]).
member(X,[_|T]):-
	member(X,T).

izograf(G1,G2,L):-
	perm_eq(G1,G2,eq_vec,L).

perm_eq([],[],_,_).
perm_eq([H1|T1],L2,Eq,L):-
	delete(H2,L2,T2),
	P =.. [Eq,H1,H2,L],
	call(P),
	perm_eq(T1,T2,Eq,L).

eq_vec([N1,T1],[N2,T2],L):-
	eq_nod(N1,N2,L),
	perm_eq(T1,T2,eq_nod,L).

eq_nod(N1,N2,L):-
	var(L),
	!,
	L = [map(N1,N2)|_].
eq_nod(N1,N2,[map(N1,N2)|_]):-
	!.
eq_nod(N1,_,[map(N1,_)|_]):-
	!,
	fail.
eq_nod(_,N2,[map(_,N2)|_]):-
	!,
	fail.
eq_nod(N1,N2,[_|T]):-
	eq_nod(N1,N2,T).