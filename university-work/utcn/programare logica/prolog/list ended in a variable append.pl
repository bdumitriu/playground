% append(L1,L2,R) - R will be list L2 appended to the end
% of list L1. All three are lists ended in a variable.

append(X,Y,Y):-var(X),!.
append([H|T],L,[H|R]):-append(T,L,R).