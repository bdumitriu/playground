% l2lev(L,R) - transforms list L (a simple list) in
% list R (a list ended in a variable).

l2lev([],_).
l2lev([H|T],[H|R]):-l2lev(T,R).