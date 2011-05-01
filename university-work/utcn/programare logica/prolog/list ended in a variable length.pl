% nrel(L,N) - N is the number of elements of list L
% (a list ended in a variable).

nrel(L,0):-var(L),!.
nrel([_|T],N):-nrel(T,Z),N is Z+1.