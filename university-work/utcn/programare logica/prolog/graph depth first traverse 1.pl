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

ltv2l(X,[]):-
	var(X),
	!.
ltv2l([H|T],[H|R]):-
	ltv2l(T,R).

travDF(X,R):-
	\+travDF_(X),
	assert(part(end)),
	collect(Rtv),
	ltv2l(Rtv,R).

travDF_(X):-
	assert(parc(X)),
	arc_o(X,Y),
	\+parc(Y),
	travDF_(Y).

collect(R):-
	getnext(X),
	!,
	cauta_insereaza_TV(X,R),
	collect(R).
collect(_).

cauta_insereaza_TV(X,[X|_]):-
	!.
cauta_insereaza_TV(X,[_|T]):-
	cauta_insereaza_TV(X,T).

getnext(X):-
	retract(parc(X)),!,X\==end.