% max_depth(T,N) - N will be the maximum depth of tree T
% (a tree ended in variables).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

max(M1,M2,M1):-M1>M2,!.
max(_,M2,M2).

max_depth(X,0):-var(X),!.
max_depth(t(_,St,Dr),M):-max_depth(St,Ms),max_depth(Dr,Md),max(Ms,Md,Mi),M is Mi+1.