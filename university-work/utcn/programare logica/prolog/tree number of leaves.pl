% leaves(T,N) - N will be the number of leaves in tree T.
% T has to be of form: t(Node,LeftSon,RightSon).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

leaves(nil,0).
leaves(t(_,nil,nil),1):-!.
leaves(t(_,S,D),N):-leaves(S,N1),leaves(D,N2),N is N1+N2.