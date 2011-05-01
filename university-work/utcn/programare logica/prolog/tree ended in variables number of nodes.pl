% nodes(T,N) - N will be the number of nodes in tree T
% (a tree ended in variables).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

nodes(X,0):-var(X),!.
nodes(t(_,St,Dr),N):-nodes(St,Ns),nodes(Dr,Nd),N is 1+Ns+Nd.