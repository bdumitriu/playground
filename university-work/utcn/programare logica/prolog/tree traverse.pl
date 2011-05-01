% pre_ord(T,L) - L will be a list containing tree T nodes in preorder.
% in_ord(T,L) - L will be a list containing tree T nodes in inorder.
% post_ord(T,L) - L will be a list containing tree T nodes in postorder.
% T has to be of form: t(Node,LeftSon,RightSon).

:-use_module(library(lists)).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

pre_ord(nil,[]).
pre_ord(t(R,S,D),L):-pre_ord(S,Ls),pre_ord(D,Ld),append([R|Ls],Ld,L).

in_ord(nil,[]).
in_ord(t(R,S,D),L):-in_ord(S,Ls),in_ord(D,Ld),append(Ls,[R|Ld],L).

post_ord(nil,[]).
post_ord(t(R,S,D),L):-post_ord(S,Ls),post_ord(D,Ld),append(Ls,Ld,L1),append(L1,[R],L).