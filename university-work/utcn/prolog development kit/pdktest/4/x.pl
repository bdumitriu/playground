x(0,X) :- !,X=a.
x(N,X) :-
	N1 is N-1,
	x(N1,X).
