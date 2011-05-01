Program de_la_pagina_3_problema_2;
uses crt;
var i,j,k,n:word;
    x,y,z:integer;

begin
clrscr;
writeln;
writeln('Acesta este un program care daca ii veti introduce un numar natural');
writeln('n va afisa toate tripletele de numere (i,j,k) care verifica conditiile:');
writeln('i la patrat+j la patrat=k la patrat si 1<i<j<k< sau = cu n.');
writeln;
write('n=');readln(n);
writeln;
writeln('Perechile de numere ce indeplinesc conditiile de mai sus sunt:');
for x:=2 to n-2 do
 begin
 i:=x;
 for y:=i+1 to n-1 do
  begin
  j:=y;
  for z:=j+1 to n do
   begin
   k:=z;
   if sqr(i)+sqr(j)=sqr(k) then writeln('(',i,',',j,',',k,')');
   end;
  end;
 end;
readkey;
end.