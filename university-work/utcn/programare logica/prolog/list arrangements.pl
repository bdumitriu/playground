% arr(L,K,R) - computes arrangements of K elements from set L into result R.

delete([X|T],X,T).
delete([H|T],X,[H|R]):-delete(T,X,R).

arr(L,K,[X|Z]):-K>0,!,delete(L,X,Y),Kp is K-1,arr(Y,Kp,Z).
arr(_,_,[]).