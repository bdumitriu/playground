% del_first(L,X,R) - deletes first occurence of X from L and stores the result into R.

del_first([X|T],X,T):-!.
del_first([H|T],X,[H|R]):-del_first(T,X,R).