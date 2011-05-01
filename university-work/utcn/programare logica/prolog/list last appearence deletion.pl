% del_last(L,X,R) - deletes the last occurence of X in L
% and stores the result into R.

del_last([X|T],X,[X|R]):-del_last(T,X,R),!.
del_last([X|T],X,T).
del_last([H|T],X,[H|R]):-del_last(T,X,R).