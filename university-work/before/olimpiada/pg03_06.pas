Program de_la_pagina_3_problema_6;
uses crt;
type sir=array[1..16000] of longint;
var n,i:integer;
    m:sir;

begin
clrscr;
writeln;
writeln('Acesta este un program care daca ii veti introduce un numar n va');
writeln('afisa primele n numere din sirul Fibonacci, sir definit de relatia:');
writeln('t[i+1]=t[i]+t[i-1].');
writeln;
write('n=');readln(n);
writeln;
writeln('Acesta sunt primele ',n,' numere din sirul Fibonacci:');
m[1]:=1;
m[2]:=2;
writeln('1');
for i:=2 to n do
 begin
 writeln(m[i]);
 m[i+1]:=m[i]+m[i-1];
 end;
readkey;
end.