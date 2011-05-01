% pretty_print(T) - pretty prints tree T.
% T has to be of form: t(Node,LeftSon,RightSon).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

ws(0,R):-write(R),write('\n'),!.
ws(N,R):-write('      '),Z is N-1,ws(Z,R).

pretty_print(nil,_).
pretty_print(t(R,S,D),A):-B is A+1,pretty_print(D,B),ws(A,R),pretty_print(S,B).

pretty_print(A):-pretty_print(A,0).