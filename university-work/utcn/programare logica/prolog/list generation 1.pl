% list_gen(N,R) - R will be something like:
%	[[1],[1,2],[1,2,3],...,[1,2,...,N]]

generate(K,N,[]):-K is N+1,!.
generate(K,N,[K|R]):-K1 is K+1,generate(K1,N,R).

generate(N,R):-generate(1,N,R).

list_gen(K,N,[]):-K is N+1,!.
list_gen(K,N,[X|R]):-generate(K,X),K1 is K+1,list_gen(K1,N,R).

list_gen(N,R):-list_gen(1,N,R).