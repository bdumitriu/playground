% sum(L,S) - adds all elements from L into sum S.

sum([],0).
sum([H|T],R):-sum(T,Z),R is Z+H.