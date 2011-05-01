% perm(L,R) - computes permutations of set L into result R.

perm([],[]).
perm(L,[X|R]):-delete(X,L,Y),perm(Y,R).