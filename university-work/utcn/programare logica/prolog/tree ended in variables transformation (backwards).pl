% tev2t(T,R) - transforms tree T (a tree ended in variables)
% into tree R (a simple tree).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

tev2t(X,nil):-var(X),!.
tev2t(t(R,St,Dr),t(R,Sm,Dm)):-tev2t(St,Sm),tev2t(Dr,Dm).