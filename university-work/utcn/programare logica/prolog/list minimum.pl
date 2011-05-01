% min(L,X) - X is the lowest value in list L.

min([H|T],M):-min(T,M),M<H,!.
min([H|_],H).