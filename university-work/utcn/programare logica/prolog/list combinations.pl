% comb(L,K,R) - computes combinations of K elements from set L into result R.

delete([X|T],X,T).
delete([_|T],X,R):-delete(T,X,R).

comb(L,K,[X|Z]):-K>0,!,delete(L,X,Y),Kp is K-1,comb(Y,Kp,Z).
comb(_,_,[]).