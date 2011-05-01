% length(L,N) - N will be the number of elements in list L.

length([],0).
length([H|T],L):-length(T,X),L is X+1.