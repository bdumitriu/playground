% depth(T,D) - D will be the length of tree T's longest branch.
% T has to be of form: t(Node,LeftSon,RightSon).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

max(X,Y,X):-X>Y,!.
max(_,Y,Y).

depth(nil,0).
depth(t(_,S,D),R):-depth(S,D1),depth(D,D2),Z1 is D1+1, Z2 is D2+1,max(Z1,Z2,R).