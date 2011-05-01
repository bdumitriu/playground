% flat(L,Ls,Le) - flattens list L. The result will be the
% difference of lists Ls and Le.

flat([],X,X).
flat([H|T],[H|P],U):-atomic(H),!,flat(T,P,U).
flat([H|T],P,U):-flat(H,P,X),flat(T,X,U).
