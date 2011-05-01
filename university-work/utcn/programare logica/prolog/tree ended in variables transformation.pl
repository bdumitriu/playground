% t2tev(T,R) - transforms tree T (a simple tree) in
% tree R (a tree ended in variables).

tree(t(7,(t(3,(t(4,(t(12,nil,nil)),nil)),(t(1,nil,(t(0,nil,nil)))))),(t(9,nil,(t(13,(t(5,nil,(t(2,nil,nil)))),nil)))))).

t2tev(nil,_).
t2tev(t(R,St,Dr),t(R,Sm,Dm)):-t2tev(St,Sm),t2tev(Dr,Dm).