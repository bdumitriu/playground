Program de_la_pagina_3_problema_4;
uses crt;
type sir=array[1..500] of integer;
var a,b:word;
    S,i,j,m,c:integer;
    x:sir;

begin
clrscr;
writeln;
writeln('Acesta este un program care daca ii veti introduce 2 numere: a si b');
writeln('va afisa toate numerele perfecte din intervalul [a,b] (un numar n e');
writeln('perfect daca este egal cu suma divizorilor lui diferiti de el;');
writeln('exemplu:6=1+2+3).');
writeln;
write('a=');readln(a);
write('b=');readln(b);
writeln;
writeln('Numerele perfecte din intervalul [',a,',',b,'] sunt:');
for j:=a to b do
 begin
 for c:=1 to 500 do
  x[c]:=0;
 m:=1;
 for i:=1 to (j div 2) do
 if j mod i=0 then
  begin
  x[m]:=i;
  m:=m+1;
  end;
 S:=0;
 for i:=1 to m do
  S:=S+x[i];
 if S=j then writeln(j);
 end;
readkey;
end.