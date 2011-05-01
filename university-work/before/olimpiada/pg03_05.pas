Program de_la_pagina_3_problema_5;
uses crt;
type sir=array[1..32000] of integer;
var x,y,a,b,i,j,m:integer;
    s:sir;

begin
clrscr;
writeln;
writeln('Acesta este un program care daca  ii veti introduce doua numere a si');
writeln('b va gasi toate numerele "prietene" din intervalul [a,b] (doua numere');
writeln('x si y sunt "prietene" daca suma divizorilor lor e egala.');
writeln;
m:=0;
write('a=');readln(a);
write('b=');readln(b);
writeln;
clrscr;
writeln('Perechiile de numere "prietene" din intervalul [',a,',',b,'] sunt:');
for i:=a to b do
 begin
 s[i]:=0;
 for j:=1 to i do
 if i mod j=0 then
  s[i]:=s[i]+j;
 end;
for i:=a to b do
for j:=i to b do
 if s[i]=s[j] then if i<>j then
  begin
  writeln('(',i,',',j,')');
  m:=m+1;
  if m=22 then
   begin
   writeln('Apasati orice tasta pentru a continua');
   readkey;
   readkey;
   clrscr;
   m:=0;
   end;
  end;
readkey;
end.