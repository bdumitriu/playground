% quicksort(L,Ls,Le) - arranges list L's elements using the
% quicksort algorithm. The result will be the difference of
% lists Ls and Le.

part(_,[],[],[]).
part(El,[H|T],[H|P1],P2):-H=<El,!,part(El,T,P1,P2).
part(El,[H|T],P1,[H|P2]):-part(El,T,P1,P2).

quicksort([],X,X).
quicksort([H|T],P,U):-part(H,T,P1,P2),quicksort(P1,P,[H|X]),quicksort(P2,X,U).