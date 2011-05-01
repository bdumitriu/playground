% tree_delete(Key,Tree,NewTree) - NewTree will be search tree Tree
% with key Key deleted from it. If Key doesn't exist in Tree then
% NewTree will be identical to Tree.

tree(t(8,(t(5,(t(4,(t(1,nil,nil)),nil)),(t(6,nil,(t(7,nil,nil)))))),(t(10,t(9,nil,nil),(t(13,(t(11,nil,(t(12,nil,nil)))),nil)))))).

tree_delete(_,nil,nil).
tree_delete(Key,t(Key,Left,nil),Left):-!.
tree_delete(Key,t(Key,nil,Right),Right):-!.
tree_delete(Key,t(NKey,Left,Right),t(NKey,NLeft,Right)):-Key<NKey,!,tree_delete(Key,Left,NLeft).
tree_delete(Key,t(NKey,Left,Right),t(NKey,Left,NRight)):-Key>NKey,!,tree_delete(Key,Right,NRight).
tree_delete(_,t(_,Left,Right),t(NKey,Left,NRight)):-successor(Right,NKey,NRight).

successor(t(Key,nil,Right),Key,Right):-!.
successor(t(Key,Left,Right),NKey,t(Key,NLeft,Right)):-successor(Left,NKey,NLeft).