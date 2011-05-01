% del_last_deep(X,L,R) - R will be deep list L without
% the last occurence of element X. Fails if X doesn't
% appear in L.

del_last_deep(X,[X|T],[X|R]):-
	del_last_deep(X,T,R),
	!.
del_last_deep(X,[X|T],T):-
	!.
del_last_deep(X,[H|T],[H|R]):-
	del_last_deep(X,T,R),
	!.
del_last_deep(X,[H|T],[R|T]):-
	\+atomic(H),
	del_last_deep(X,H,R).