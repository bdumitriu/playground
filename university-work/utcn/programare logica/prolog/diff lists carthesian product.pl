% cart_prod(L1,L2,Ls,Le) - computes the carthesian product of 
% lists L1 and L2. The result will be the difference of lists 
% Ls and Le.

pairs(_,[],X,X).
pairs(El,[H|T],[[El,H]|X],U):-pairs(El,T,X,U).

cart_prod([],_,X,X).
cart_prod([H|T],L,P,U):-pairs(H,L,P,X),cart_prod(T,L,X,U).