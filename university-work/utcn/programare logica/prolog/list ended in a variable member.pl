% member(X,L) - ends with success if X is a member of list
% L (a list ended in a variable).

member(_,T):-var(T),!,fail.
member(X,[X|_]).
member(X,[H|T]):-member(X,T).
