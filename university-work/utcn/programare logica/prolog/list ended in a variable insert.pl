% insertLEV(X,L) - inserts element X is list L
% (a list ended in a variable).

insertLEV(X,L):-var(L),!,L = [X|_].
insertLEV(X,[X|_]):-!.
insertLEV(X,[_|T]):-insertLEV(X,T).