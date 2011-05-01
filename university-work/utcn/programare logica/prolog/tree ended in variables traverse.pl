% in_ord(T,R) - R will be a list containing the nodes of
% tree T (a tree ended in variables) in inorder.

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

append(X,Y,Y):-var(X),!.
append([H|T],L,[H|R]):-append(T,L,R).

in_ord(X,[]):-var(X),!.
in_ord(t(R,St,Dr),Z):-in_ord(St,Ts),in_ord(Dr,Td),append(Ts,[R|Td],Z).