% reverse(L,R) - R will be list L (a list ended in a variable)
% reversed with the same variable at the end.

l2ltv_spec([],X,X).
l2ltv_spec([H|T],X,[H|R]):-l2ltv_spec(T,X,R).

append(X,L,R):-var(X),l2ltv_spec(L,X,R),!.
append([H|T],L,[H|R]):-
	append(T,L,R).

reverse(X,X):-var(X),!.
reverse([H|T],R):-reverse(T,Ti),append(Ti,[H],R).