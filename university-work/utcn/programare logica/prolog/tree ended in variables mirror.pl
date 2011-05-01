% mirror(T,R) - R will be a tree ended in variables which
% is obtained by mirroring tree T (also ended in variables).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

mirror(X,_):-var(X),!.
mirror(t(R,St,Dr),t(R,Do,So)):-mirror(St,So),mirror(Dr,Do).