% pre_ord(T,Ls,Le) - Ls-Le will be a list containing tree T nodes in preorder.
% in_ord(T,Ls,Le) - Ls-Le will be a list containing tree T nodes in inorder.
% post_ord(T,Ls,Le) - Ls-Le will be a list containing tree T nodes in postorder.
% T has to be of form: t(Node,LeftSon,RightSon).


% cart_prod(L1,L2,Ls,Le) - computes the carthesian product of 
% lists L1 and L2. The result will be the difference of lists 
% Ls and Le.

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

pre_ord(nil,X,X).
pre_ord(t(R,S,D),[R|P],U):-pre_ord(S,P,X),pre_ord(D,X,U).

in_ord(nil,X,X).
in_ord(t(R,S,D),P,U):-in_ord(S,P,[R|X]),in_ord(D,X,U).

post_ord(nil,X,X).
post_ord(t(R,S,D),P,U):-post_ord(S,P,X),post_ord(D,X,[R|U]).