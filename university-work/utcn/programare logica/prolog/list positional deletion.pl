% delete(L,K,X,R) - deletes one occurence of X from L and stores the
% result into R if and only if this occurence is after the K-th 
% position in the list.

delete([X|T],Pos,K,X,T):-Pos>=K.
delete([H|T],Pos,K,X,[H|R]):-P is Pos+1,delete(T,P,K,X,R).

delete(L,K,X,R):-delete(L,1,K,X,R).