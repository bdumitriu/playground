Program de_la_pagina_3_problema_3;
uses crt;
type sir=array[1..500] of integer;
var n,m:word;
    i,S:integer;
    a:sir;

begin
clrscr;
writeln;
writeln('Acesta este un program care daca ii vati introduce un numar natural n');
writeln('va verifica daca numarul n este perfect sau nu (un numar n e perfect');
writeln('daca este egal cu suma divizorilor lui diferiti de el; exemplu:6=1+2+3).');
writeln;
write('n=');readln(n);
m:=1;
for i:=1 to (n div 2) do
 if n mod i=0 then
  begin
  a[m]:=i;
  m:=m+1;
  end;
S:=0;
for i:=1 to m do
 S:=S+a[i];
writeln;
if S=n then writeln('Numarul ',n,' este perfect.')
       else writeln('Numarul ',n,' nu este perfect.');
readkey;
end.