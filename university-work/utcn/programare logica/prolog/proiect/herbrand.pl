%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% list of predicates not to be reported as belonging %
% to the program.                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
predefined_predicates(['.'/2, var/1, nonvar/1, atomic/1, is/2, (+)/2, < /2, > /2, =< /2, >= /2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% list of constants not to be reported as belonging %
% to the program.                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
predefined_constants([!, fail]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% You should load a file by load_file(Filename) %
% instead of any other way in order to obtain a %
% correct Herbrand universe, Herbrand base,...  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_file(Filename):-
	reset,
	consult(Filename).

reset:-
	abolish(predicate),
	abolish(simple_constant),
	abolish(functional_constant).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some necessary well known predicates. %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
append([],L,L).
append([H|T],L,[H|R]):-
	append(T,L,R).

member(X,[X|_]):-!.
member(X,[_|T]):-
	member(X,T).

delete([X|T],X,T).
delete([_|T],X,R):-
	delete(T,X,R).

reverse([],[]).
reverse([H|T],R):-
	reverse(T,Tr),append(Tr,[H],R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comb(L,K,R) => R will be a combination of %
% K elements from L.                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
comb(L,K,[X|Z]):-
	K>0,
	!,
	delete(L,X,Y),
	Kp is K-1,
	comb(Y,Kp,Z).
comb(_,_,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Herbrand Universe function. hu(N,Hu) will put in       %
% Hu all the elements of the Herbrand universe up to the     %
% Nth level. Level 1 means f(constant), level 2 means        %
% f(f(constant)) and so on... If the Herbrand universe is    %
% finite then Hu will containt the list of simple constants, %
% no matter what the value of N is.                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hu(N,Hu):-
	abolish(c_list),
	assert(c_list([])),
	s_constants,
	retract(c_list(CL)),
	reverse(CL,R),
	assert(c_list(R)),
	hu_up_to_level(N),
	retract(c_list(Hu)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The Herbrand base function. hb(N,Hb) will create the %
% Herbrand base using all predicates and the result of %
% hu(N,Hu).                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hb(N,Hb):-
	hu(N,Hu),
	abolish(p_list),
	assert(p_list([])),
	hb_up_to_level(Hu),
	retract(p_list(Temp)),
	reverse(Temp,Hb).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creates all elements of the Herbrand universe up to the %
% Nth level.                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hu_up_to_level(N):-
	N>0,
	!,
	assert(temp_list([])),
	f_constants,
	retract(temp_list(TL)),
	reverse(TL,R),	
	retract(c_list(CL)),
	append(CL,R,NewCL),
	assert(c_list(NewCL)),
	Np is N-1,
	hu_up_to_level(Np).
hu_up_to_level(0):-
	!.
hu_up_to_level(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creates all elements of the Herbrand base up to the %
% Nth level using the elements in Hu.                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hb_up_to_level(Hu):-
	clause(predicate(X/Y),_),
	comb(Hu,Y,R),
	T =.. [X|R],
	retract(p_list(C)),
	assert(p_list([T|C])),
	fail.
hb_up_to_level(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% asserts El in temp_list if El is not a member %
% of CL, where c_list(CL).                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_if_not_member(El,Temp):-
	c_list(CL),
	member(El,CL),
	assert(temp_list(Temp)),
	!.
assert_if_not_member(El,Temp):-
	assert(temp_list([El|Temp])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creates a list containing all the simple constants of %
% a program by collecting all simple_constant/1 clauses %
% previously asserted into the database.                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
s_constants:-
	clause(simple_constant(X),_),
	retract(c_list(Y)),
	assert(c_list([X|Y])),
	fail.
s_constants:-
	c_list([]),
	retract(c_list([])),
	assert(c_list([a])).
s_constants.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adds a new level to the elements of the Herbrand universe %
% by collecting all the functional constants of the program %
% previously asserted as functional_constant/1 and forming  %
% new elements with combinations of all existing elements.  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
f_constants:-
	clause(functional_constant(X/Y),_),
	c_list(CL),
	comb(CL,Y,R),
	T =.. [X|R],
	retract(temp_list(C)),	
	assert_if_not_member(T,C),
	fail.
f_constants.

%
%
%
%create_predicates(Hu):-
%	clause(predicate(X/Y),_),
%	comb(Hu,Y,R),
%	T =.. [X|R],
%	retract(p_list(C)),
%	assert(p_list([T|C])),
%	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assert_p_if_necessary(PredicateName,Arity) =>     %
% asserts the clause predicate(PredicateName/Arity) %
% if such a clause does not already exist and if    %
% PredicateName/Arity is not a member of L, where   %
% predefined_predicates(L).                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_p_if_necessary(P,N):-
	predefined_predicates(PL),
	member(P/N,PL),
	!.
assert_p_if_necessary(P,N):-
	clause(predicate(P/N),_),
	!.
assert_p_if_necessary(P,N):-
	assert(predicate(P/N)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assert_fc_if_necessary(FunctionalConstant,Arity) =>              %
% asserts the clause functional_constant(FunctionalConstant/Arity) %
% if such a clause does not already exist and if                   %
% FunctionalConstant/Arity is not a member of L, where             %
% predefined_predicates(L).                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_fc_if_necessary(FC,N):-
	predefined_predicates(PL),
	member(FC/N,PL),
	!.
assert_fc_if_necessary(FC,N):-
	clause(functional_constant(FC/N),_),
	!.
assert_fc_if_necessary(FC,N):-
	assert(functional_constant(FC/N)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assert_c_if_necessary(Constant) =>              %
% asserts the clause simple_constant(Constant) if %
% such a clause does not already exist and if     %
% Constant is not a member of L, where            %
% predefined_constants(L).                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
assert_c_if_necessary(C):-
	predefined_constants(PC),
	member(C,PC),
	!.
assert_c_if_necessary(C):-
	clause(simple_constant(C),_),
	!.
assert_c_if_necessary(C):-
	assert(simple_constant(C)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% process(List,ProcessedList,_) => does a lot of stuff %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process([A],[A],_):-
	var(A),
	!.
process([A],[A],_):-
	nonvar(A),
	A =.. LA,
	length(LA,1),
	!,
	assert_c_if_necessary(A).
process([A],R,F):-
	var(F),
	nonvar(A),
	!,
	A =.. LA,
	[H|T] = LA,
	length(LA,N),
	N1 is N-1,
	assert_p_if_necessary(H,N1),
	process(T,R,1).
process([A],R,F):-
	nonvar(F),
	nonvar(A),
	A =.. LA,
	length(LA,N),
	N1 is N-1,
	[H|T] = LA,
	!,
	assert_fc_if_necessary(H,N1),
	process(T,R,1).
process([A|B],R,F):-
	process([A],FA,F),
	process(B,FB,F),
	append(FA,FB,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% make_list((1,2,3),R) => R will be [1,2,3]. %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_list((A,B),[A|C]):-
	make_list(B,C).
make_list((A),[A]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These are the means to process the program while %
% loading.                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user:term_expansion((?-(Question)),(?-(Question))):-!.

user:term_expansion((:-(Directive)),(:-(Directive))):-!.

user:term_expansion('end_of_file','end_of_file'):-!.

user:term_expansion((Head:-Body),(Head:-Body)):-
	make_list(Body,BodyList),
	process([Head|BodyList],_,_).

user:term_expansion((Head),(Head)):-
	process([Head],_,_).