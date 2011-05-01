:-dynamic my/1.
:-use_module(library(lists)).

l:-listing([my]).
c:-abolish(my/3).

proc((A,B), [A|C]):-proc(B,C).
proc((H),[H]).

napp([],[]).
napp([A|B],R):-napp(B,R1),append(A,R1,R).

flat([],[]).
flat([A],[A]) :- atomic(A).
flat([A],[A]) :- var(A).
flat([A],[H/NT|R]) :- 
	A =.. [H|T], 
	length(T,NT),
	flat(T,R).
flat([A|B],R) :-
	flat([A],R1), 
	flat(B,R2), 
	append(R1,R2,R).
	
split([],[],[],[]).
split([H|T],[H|X],Y,Z):-atomic(H),split(T,X,Y,Z).
split([H|T],X,[H|Y],Z):-var(H),split(T,X,Y,Z).
split([F/A|T],X,Y,[F/A|Z]):-split(T,X,Y,Z).

split([],[],[]).
split([X/Y|T],[X|A],[Y|B]):-split(T,A,B).

cnt(_,[],[]).
cnt(N,[H|T],[H-N|T1]):-N1 is N+1, cnt(N1,T,T1).

subst([],_,[]).
subst([H|T],L,[N|R]):-member(H-N,L),!,subst(T,L,R).
subst([H|T],L,[0|R]):-subst(T,L,R).

wl(_,[]).
wl(S,[H|T]):-write(S,H),write(S,' '),wl(S,T).

wl1(_,[]).
wl1(S,[H|T]):-put(S,H),wl1(S,T).

final:- 
	findall(X, my(_,X,_),R), 
	napp(R,R1),
	%write(R1),nl,nl,
	remove_duplicates(R1,R2),
	%write(R2),nl,
	split(R2, Atoms, Vars, Terms),
	write(Atoms),nl,
	length(Vars, VV),
	write(VV),nl,
	%write(Vars),nl,%write(Terms),nl,
	split(Terms, F,A),
	write(F),nl,write(A),nl,
	append(Atoms, Vars, O),
	append(O,Terms,OO),
	%write(OO),nl,
	cnt(0,OO,U),
	%write(U),nl,
	subst(R1,U,R3),
	write(R3),nl,
	% spit to file
	open('test',write,S,[type(binary)]),
	wl(S,Atoms),
	write(S,VV),write(S,' '),
	wl(S,F),
	wl1(S,A),
	wl1(S,R3),
	close(S)
	.
	
	


user:term_expansion('end_of_file', 'end_of_file'):-
	write('Reached end of file'),
	nl,
	l,
	final.
	
user:term_expansion(?-(A),?-(A)):-
	write('Good question !'),
	nl.

user:term_expansion((A:-B), (A:-B)):-
	proc(B,LB),
	flat([A|LB],F),
	length(F,N),
	assert(my([A,':-'|LB],F,N)).

user:term_expansion((A),(A)):-
	flat([A],F),
	length(F,N),
	assert(my([A],F,N)).	


