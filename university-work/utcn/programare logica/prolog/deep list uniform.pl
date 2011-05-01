% unif(L,R) - R is an uniform representation of list L,
% meaning that it will contain all atomic elements found
% in L and the atomic elements found in uniform 
% representations of L's non atomic elements.

append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).

unif([],[]).
unif([H|T],[H|R]):-atomic(H),!,unif(T,R).
unif([H|T],R):-unif(H,R1),unif(T,R2),append(R1,R2,R).

% a different way to do the same thing.
%unif([],[]):-!.
%unif(H,[H]):-
%	atomic(H).
%unif([H|T],R):-
%	unif(H,R1),
%	unif(T,R2),
%	append(R1,R2,R).