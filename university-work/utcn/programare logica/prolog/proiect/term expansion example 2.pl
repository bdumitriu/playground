reset:-
	abolish(pdetails).

list_info:-
	\+(clause(pdetails(_,_),_)),
	write('No predicates loaded since last reset_count.'),
	nl,
	!.
list_info:-
	pdetails(X,Y),
	write(X),
	write(' has '),
	write(Y),
	write(' clauses.'),
	nl,
	fail.

load_file(Filename):-
	reset,
	consult(Filename),
	list_info.

user:term_expansion((?-(Question)),(?-(Question))):-!.

user:term_expansion((:-(Directive)),(:-(Directive))):-!.

user:term_expansion('end_of_file','end_of_file'):-!.

user:term_expansion((Head:-Body),(Head:-Body)):-
	functor(Head,Name,Arity),
	retract(pdetails(Name/Arity,Nr)),
	!,
	NewNr is Nr+1,
	assert(pdetails((Name/Arity),NewNr)).

user:term_expansion((Head:-Body),(Head:-Body)):-
	!,
	functor(Head,Name,Arity),
	assert(pdetails(Name/Arity,1)).

user:term_expansion((Head),(Head)):-
	functor(Head,Name,Arity),
	retract(pdetails(Name/Arity,Nr)),
	!,
	NewNr is Nr+1,
	assert(pdetails((Name/Arity),NewNr)).

user:term_expansion((Head),(Head)):-
	!,
	functor(Head,Name,Arity),
	assert(pdetails(Name/Arity,1)).