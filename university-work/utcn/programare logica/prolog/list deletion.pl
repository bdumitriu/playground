% delete(L,X,R) - deletes one occurence of X from L and stores the result into R.

delete([X|T],X,T).
delete([H|T],X,[H|R]):-delete(T,X,R).