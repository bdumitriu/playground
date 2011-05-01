% at_last(L,R) - R will be the list of all atomic elements
% that are on the last position either in L or in any of
% its sublists down to any level of imbrication.

append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).

at_last([],[X],X):-nonvar(X).
at_last([],[],X):-var(X).
at_last([H|T],R,_):-atomic(H),!,at_last(T,R,H).
at_last([H|T],R,_):-at_last(H,R1,_),at_last(T,R2,_),append(R1,R2,R).

at_last(L,R):-at_last(L,R,_).

% a nicer solution, without using a flag.
%at_last([H|T],[H]):-
%	atomic(H),
%	atomic(T),
%	!.
%at_last([H|T],R):-
%	atomic(H),
%	!,
%	at_last(T,R).
%at_last([H|T],R):-
%	atomic(T),
%	!,
%	at_last(H,R).
%at_last([H|T],R):-
%	at_last(H,R1),
%	at_last(T,R2),
%	append(R1,R2,R).