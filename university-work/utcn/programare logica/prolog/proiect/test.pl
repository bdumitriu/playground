barbat(ion).
barbat(vasile).
barbat(mihai).
barbat(mircea).

p(nil,nil):-atomic(a),atomic(a,5).
p(A,B):-p(f(a),f([B,c|_],5)),q(g(A)).
q(a).
q(A):-A=<2, var(A).

