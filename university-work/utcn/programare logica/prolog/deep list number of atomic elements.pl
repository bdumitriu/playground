% nr_elem(L,R) - R will be the number of atomic elements in
% L's uniform representation (see "list uniform.pl" for 
% further details.

nr_elem([],0).
nr_elem([H|T],R):-atomic(H),!,nr_elem(T,R1),R is 1+R1.
nr_elem([H|T],R):-nr_elem(H,R1),nr_elem(T,R2),R is R1+R2.