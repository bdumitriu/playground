% int_nodes(T,N) - N will be the number of nodes
% in tree T that are not leaves.
% T has to be of form: t(Node,LeftSon,RightSon).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

int_nodes(nil,0).
int_nodes(t(_,nil,nil),0):-!.
int_nodes(t(_,S,D),R):-int_nodes(S,R1),int_nodes(D,R2),R is R1+R2+1.