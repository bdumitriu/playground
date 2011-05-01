% bfs(X,Y,R) - R will be the list of the graph's node
% in breadth first order, with X as the starting node
% and Y as the finishing node.

:-dynamic(parc/1).

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
arc_o(h,f).

member(X,[X|_]).
member(X,[_|T]):-
	member(X,T).

reverse([],L,L).
reverse([H|T],U,V):-
	reverse(T,U,[H|V]).

bfs(X,Y,R):-	
	bfs([X|_],Y,[],Ri),
	reverse(Ri,R,[]).

bfs(Cand,_,_,_):-
	var(Cand),
	!,
	fail.
bfs([Y|_],Y,Exp,[Y|Exp]):-
	!.
bfs([X|Cand],Y,Exp,R):-
	expand(X,Cand,Exp),
	bfs(Cand,Y,[X|Exp],R).

expand(X,_,Exp):-
	arc_o(X,Y),
	\+member(Y,Exp),
	assert(parc(Y)),
	fail.
expand(_,Cand,_):-
	assert(parc(end)),
	collect(Cand).

collect(Cand):-
	getnext(X),
	!,
	cauta_insereaza_TV(X,Cand),
	collect(Cand).
collect(_).

getnext(X):-
	retract(parc(X)),!,X\==end.

cauta_insereaza_TV(X,[X|_]):-
	!.
cauta_insereaza_TV(X,[_|T]):-
	cauta_insereaza_TV(X,T).