% list_gen(N,M,R) - R will be something like:
%	[[1,1,..,1],[2,2,...,2],...,[N,N,...,N]]
% with M elements in each list of the type
%	[K,K,...,K].

generate(M,K,_,[]):-K is M+1,!.
generate(M,K,N,[N|R]):-K1 is K+1,generate(M,K1,N,R).

generate(N,M,R):-generate(M,1,N,R).

list_gen(K,_,M,[]):-K is M+1,!.
list_gen(K,N,M,[X|R]):-generate(K,N,X),K1 is K+1,list_gen(K1,N,M,R).

list_gen(N,M,R):-list_gen(1,N,M,R).