% at_first(L,R) - R will be the list of all atomic elements
% that are on the first position either in L or in any of
% its sublists down to any level of imbrication.

append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).

at_first([],[],_).
at_first([H|T],[H|R],F):-var(F),atomic(H),!,F=1,at_first(T,R,F).
at_first([H|T],R,F):-nonvar(F),atomic(H),!,at_first(T,R,F).
at_first([H|T],R,F):-at_first(H,R1,_),at_first(T,R2,1),append(R1,R2,R).

at_first(L,R):-at_first(L,R,_).

%afp1([],[]):-!.
%afp1(H,[H]):-
%	atomic(H),	
%	!.
%afp1([H|T],R):-
%	afp1(H,R1),
%	afp2(T,R2),
%	append(R1,R2,R).
%
%afp2([],[]).
%afp2([H|T],R):-
%	atomic(H),
%	!,
%	afp2(T,R).
%afp2([H|T],R):-
%	afp1(H,R1),
%	afp2(T,R2),
%	append(R1,R2,R).
%
%at_first(L,R):-afp1(L,R).