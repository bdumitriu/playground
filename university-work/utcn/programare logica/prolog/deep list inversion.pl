% deep_inv(L,R) - R will be the inversion of L, with all
% lists nested in L deeply inverted too.

deep_inv([],[]).
deep_inv([H|T],R):-atomic(H),!,deep_inv(T,TI),append(TI,[H],R).
deep_inv([H|T],R):-deep_inv(H,HI),deep_inv(T,TI),append(TI,[HI],R).