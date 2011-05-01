% inssort(UL, SL) - sorts the list UL into SL using insertion sort.

ord(X,Y):-X<Y.

elemins(X,[],[X]).
elemins(X,[H|T],[X,H|T]):-ord(X,H),!.
elemins(X,[H|T],[H|R]):-elemins(X,T,R).

inssort([],R,R).
inssort([H|T],P,R):-elemins(H,P,Y),inssort(T,Y,R).

inssort(L,R):-inssort(L,[],R).