% transf_edge_n_list - transforms an edge representation of a
% graph in a neighbor list representation of the same graph.

:-dynamic(edge/2).
:-dynamic(n_list/2).

edge(a,b).
edge(a,c).
edge(a,d).
edge(b,e).
edge(b,f).
edge(c,b).
edge(c,e).
edge(d,e).
edge(e,f).
edge(g,g).


create_empty_n_list1:-edge(X,_),\+(clause(n_list(X,_),_)),assert(n_list(X,[])),fail.
create_empty_n_list1.

create_empty_n_list2:-edge(_,X),\+(clause(n_list(X,_),_)),assert(n_list(X,[])),fail.
create_empty_n_list2.

create_empty_n_list3:-edge(nil,X),\+(clause(n_list(X,_),_)),assert(n_list(X,[])),fail.
create_empty_n_list3.

transf_edge_n_list_:-edge(X,Y),retract(n_list(X,L)),assert(n_list(X,[Y|L])),fail.
transf_edge_n_list_.

transf_edge_n_list:-
	abolish(n_list),
	create_empty_n_list1,
	create_empty_n_list2,
	create_empty_n_list3,
	transf_edge_n_list_.