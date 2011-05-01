Program de_la_pagina_2_problema_1;
uses crt;
var i,j,k,x:word;
    n:integer;
                                                                      
begin
clrscr;
writeln;
writeln('Acesta este un program care daca-i veti introduce 3 numere naturale:');
writeln('i,j si k va determina restul impartirii numarului i la puterea j la k.');
writeln;
write('i=');readln(i);
write('j=');readln(j);
write('k=');readln(k);
n:=1;
for x:=1 to j do
 n:=n*i;
n:=n mod k;
writeln;
writeln('Restul impartirii numarului i la puterea j la k este ',n,'.');
readkey;
end.
