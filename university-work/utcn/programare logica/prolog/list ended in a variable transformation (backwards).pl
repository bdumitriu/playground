% lev2l(L,R) - transforms list L (a list ended in a variable)
% into list R (a simple list).

lev2l(L,[]):-var(L),!.
lev2l([H|T],[H|R]):-lev2l(T,R).