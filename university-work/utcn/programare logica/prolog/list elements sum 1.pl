% sum(L,S) - adds all elements from L into sum S.

sum([],R,R).
sum([H|T],Ac,S):-Z is Ac+H,sum(T,Z,S).

sum(L,S):-sum(L,0,S).