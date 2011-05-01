% delete(L,X,R) - deletes one occurence of X from L and stores the result into R.

del_all([X|T],X,R):-del_all(T,X,R),!.
del_all([X|T],X,T).
del_all([H|T],X,[H|R]):-del_all(T,X,R).