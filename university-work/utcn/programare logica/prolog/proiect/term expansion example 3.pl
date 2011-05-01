reset:-
	abolish(pdetails),
	abolish(pcount).

list_info:-
	\+(clause(pdetails(_,_,_),_)),
	write('No predicates loaded since last reset_count.'),
	nl,
	!.
list_info:-
	pdetails(X,Y,Z),
	write('Predicate name:\t\t'),
	write(X),
	write('.'),
	nl,
	write('Nr. of clauses:\t\t'),
	write(Y),
	write('.'),
	nl,
	write('Largest clause has:\t'),
	write(Z),
	write(' predicate calls.'),
	nl,
	nl,
	fail.
list_info:-
	pcount(X),
	write('Total nr. of predicates loaded: '),
	write(X),
	write('.'),
	nl.

make_list((A,B),[A|C]):-
	make_list(B,C).
make_list((A),[A]).

list_length([],0).
list_length([_|T],R):-
	list_length(T,Rt),R is Rt+1.

max_value(X,Y,X):-
	X>Y,!.
max_value(_,Y,Y).

load_file(Filename):-
	reset,
	assert(pcount(0)),
	consult(Filename),
	list_info.	

user:term_expansion((?-(Question)),(?-(Question))):-!.

user:term_expansion((:-(Directive)),(:-(Directive))):-!.

user:term_expansion('end_of_file','end_of_file'):-!.

user:term_expansion((Head:-Body),(Head:-Body)):-
	functor(Head,Name,Arity),
	retract(pdetails(Name/Arity,Nr,CurrentMax)),
	!,
	NewNr is Nr+1,
	make_list(Body,Bodylist),
	list_length(Bodylist,Length),
	max_value(CurrentMax,Length,Max),
	assert(pdetails((Name/Arity),NewNr,Max)).

user:term_expansion((Head:-Body),(Head:-Body)):-
	!,
	functor(Head,Name,Arity),
	make_list(Body,Bodylist),
	list_length(Bodylist,Length),
	assert(pdetails(Name/Arity,1,Length)),
	retract(pcount(Count)),
	NewCount is Count+1,
	assert(pcount(NewCount)).

user:term_expansion((Head),(Head)):-
	functor(Head,Name,Arity),
	retract(pdetails(Name/Arity,Nr,Max)),
	!,
	NewNr is Nr+1,
	assert(pdetails((Name/Arity),NewNr,Max)).

user:term_expansion((Head),(Head)):-
	!,
	functor(Head,Name,Arity),
	assert(pdetails(Name/Arity,1,0)),
	retract(pcount(Count)),
	NewCount is Count+1,
	assert(pcount(NewCount)).