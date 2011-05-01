% Symbolic differentiation of an expression.

eval(A+0,A):-!.
eval(0+A,A):-!.
eval(_*0,0):-!.
eval(0*_,0):-!.
eval(A*1,A):-!.
eval(1*A,A):-!.
eval(A/1,A):-!.
eval(A+B,A+B):-!.
eval(A*B,A*B):-!.
eval(A/B,A/B):-!.

simplify(A+B,R):-simplify(A,Sa),simplify(B,Sb),eval(Sa+Sb,R).
simplify(A*B,R):-simplify(A,Sa),simplify(B,Sb),eval(Sa*Sb,R).
simplify(A/B,R):-simplify(A,Sa),simplify(B,Sb),eval(Sa/Sb,R).
simplify(A,A):-atomic(A).

d(X,X,1):-atomic(X),!.
d(X,_,0):-atomic(X),!.
d(F+G,X,A+B):-d(F,X,A),d(G,X,B).
d(F*G,X,A*G+F*B):-d(F,X,A),d(G,X,B).
d(F/G,X,(A*G-F*B)/(G*G)):-d(F,X,A),d(G,X,B).
d(ln(F),X,A/F):-d(F,X,A).

diff(E,X,D):-d(E,X,R),simplify(R,D).