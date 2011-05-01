% bf(T,L) - L will be a list containing tree T's nodes breadth first.
% T has to be of form: t(Node,LeftSon,RightSon).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

p1(1,[],[],0).
p1(1,[H|T],[H|R],M):-!,p1(1,T,R,N),M is N+1.
p1(N,[_|T],R,M):-Z is N-1,p1(Z,T,R,M).

p2([],[],0).
p2([t(_,nil,nil)|T],Y,N):-!,p2(T,Y,N).
p2([t(_,S,nil)|T],[S|Y],N):-!,p2(T,Y,Z),N is Z+1.
p2([t(_,nil,D)|T],[D|Y],N):-!,p2(T,Y,Z),N is Z+1.
p2([t(_,S,D)|T],[S,D|Y],N):-p2(T,Y,Z),N is Z+2.

bf(N,L,R):-p1(N,L,X,M),p2(X,Y,_),M\==0,!,append(L,Y,Z),C is M+N,bf(C,Z,R).
bf(_,R,R).

make_list([],[]).
make_list([t(R,_,_)|T],[R|X]):-make_list(T,X).

bf(t(R,S,D),X):-bf(1,[t(R,S,D)],L),make_list(L,X).