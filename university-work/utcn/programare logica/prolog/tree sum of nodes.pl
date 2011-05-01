% sum(T,S) - S will be the sum of all nodes in tree T.
% T has to be of form: t(Node,LeftSon,RightSon).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

sum(nil,0).
sum(t(_,S,D),Sum):-sum(S,S1),sum(D,S2),Sum is S1+S2+1.