% dft - traverses graph in depth first order.

edge(a,b).
edge(a,c).
edge(a,d).
edge(b,f).
edge(b,e).
edge(c,b).
edge(c,e).
edge(d,e).
edge(e,f).
edge(g,g).

append([],L,L).
append([H|T],L,[H|R]):-append(T,L,R).

reverse([],[]).
reverse([H|T],R):-reverse(T,R1),append(R1,[H],R).

node(X):-edge(X,_);edge(_,X).

v_list:-
	node(X),
	\+(clause(vertex(X),_)),
	assert(vertex(X)),
	fail.
v_list.

vertex_list_(L,R):-
	retract(vertex(X)),
	vertex_list_(L,[X|R]).
vertex_list_(L,L).

vertex_list(L):-
	v_list,
	vertex_list_(L,[]),
	!.

find_unvisited(H,[H|_]):-
	\+(clause(visited(H),_)).
find_unvisited(X,[H|T]):-
	clause(visited(H),_),
	find_unvisited(X,T).

depth_tree(X):-
	edge(X,Z),
	\+(clause(visited(Z),_)),
	assert(visited(Z)),
	depth_tree(Z).
depth_tree(_).

print_all:-
	visited(X),
	write(X),
	write(' '),
	fail.
print_all.

dft:-
	abolish(visited),
	abolish(vertex),
	abolish(vertices),
	vertex_list(L),
	reverse(L,L1),
	assert(vertices(L1)),
	dft_,
	print_all.

dft_:-
	vertices(L),
	find_unvisited(X,L),
	assert(visited(X)),
	depth_tree(X),
	fail.
dft_.