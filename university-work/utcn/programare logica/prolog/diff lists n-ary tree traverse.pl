% pre_ord(T,R) - lists the nodes of tree T in preorder.
% post_ord(T,R) - lists the nodes of tree T in postorder.
% T has to be of form: t(Node,ListOfSonsFromLeftToRight).

tree(t(2,[t(4,[t(1,[]),t(5,[]),t(7,[])]),t(3,[t(9,[]),t(1,[]),t(10,[])]),t(7,[t(4,[]),t(5,[]),t(6,[])])])).

pre_ord(t(R,[]),[R|X],X,F):-F==1,!.
pre_ord(t(_,[]),X,X,_).
pre_ord(t(R,[H|T]),[R|P],U,F):-F==1,!,pre_ord(H,P,X,1),pre_ord(t(R,T),X,U,0).
pre_ord(t(R,[H|T]),P,U,_):-pre_ord(H,P,X,1),pre_ord(t(R,T),X,U,0).

post_ord(t(R,[]),[R|X],X).
post_ord(t(R,[H|T]),P,U):-post_ord(H,P,X),post_ord(t(R,T),X,U).

pre_ord(T,R):-pre_ord(T,R,[],1).
post_ord(T,R):-post_ord(T,R,[]).