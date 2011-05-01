Program de_la_pagina_3_problema_9;
uses crt;
label 1;
var m,n,i,k,l,j,x,y:integer;
    a,b:array[1..3000] of integer;
    prime:boolean;

begin
 clrscr;
 writeln('Acesta este un program care daca ii veti introduce doua numar n si ');
 writeln('m va va afisa numarul de numere dintre 1 si m relativ prime cu n');
 write('n=');readln(n);
 write('m=');readln(m);
 writeln;
 k:=1;
 for i:=2 to n do
  if n mod i = 0 then
                  begin
                   a[k]:=i;
                   k:=k+1;
                  end;
 writeln('Nr. relativ prime cu ',n,' sunt : ');
 l:=1;
 for i:=2 to m do
  begin
   for j:=2 to i do
    if i mod j = 0 then
                    begin
                     b[l]:=j;
                     l:=l+1;
                    end;
   for x:=1 to k-1 do
    for y:=1 to l-1 do
     if a[x]=b[y] then
                   begin
                    prime:=false;
                    goto 1;
                   end
                  else
                   prime:=true;
   1:if prime then write(' ',i);
   for x:=1 to l-1 do
    b[x]:=0;
  end;
 readkey;
end.