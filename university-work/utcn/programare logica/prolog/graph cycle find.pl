% cycle(X,R) - R will be a cycle starting and ending with X.
% Fails if no such cycle exists.

edge(a,b).
edge(a,h).
edge(b,a).
edge(b,c).
edge(b,f).
edge(b,g).
edge(c,d).
edge(d,b).
edge(d,c).
edge(e,d).
edge(e,f).
edge(e,g).
edge(f,a).
edge(f,c).
edge(f,d).
edge(f,e).
edge(f,g).
edge(g,a).
edge(g,h).
edge(h,b).
edge(h,f).

member(X,[X|_]).
member(X,[_|T]):-
	member(X,T).

reverse([],L,L).
reverse([H|T],U,V):-
	reverse(T,U,[H|V]).

cycle(X,Y,L,L):-X\==Y,edge(Y,X).
cycle(X,Y,Lparc,R):-
	edge(Y,Z),
	\+member(Z,Lparc),
	cycle(X,Z,[Z|Lparc],R).

cycle(X,R):-
	cycle(X,X,[X],Ri),
	reverse([X|Ri],R,[]).