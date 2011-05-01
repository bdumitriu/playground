% single_son(T,N) - N will be the number of single nodes in tree T.
% T has to be of form: t(Node,LeftSon,RightSon).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

single_son(nil,0).
single_son(t(_,nil,nil),0):-!.
single_son(t(_,S,nil),R):-!,single_son(S,N),R is N+1.
single_son(t(_,nil,D),R):-!,single_son(D,N),R is N+1.
single_son(t(_,S,D),N):-single_son(S,N1),single_son(D,N2),N is N1+N2.