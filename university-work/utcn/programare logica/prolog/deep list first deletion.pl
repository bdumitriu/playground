% del_first_deep(X,L,R) - R will be deep list L without
% the first occurence of element X. Fails if X doesn't
% appear in L.

del_first_deep(X,[X|T],T):-
	!.
del_first_deep(X,[H|T],[R|T]):-
	\+atomic(H),
	del_first_deep(X,H,R),
	!.
del_first_deep(X,[H|T],[H|R]):-
	del_first_deep(X,T,R).