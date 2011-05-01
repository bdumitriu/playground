% reflex(T,Rt) - Rt will be tree T in a mirror.
% T has to be of form: t(Node,LeftSon,RightSon).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

reflex(nil,nil).
reflex(t(R,S,D),t(R,RD,RS)):-reflex(S,RS),reflex(D,RD).