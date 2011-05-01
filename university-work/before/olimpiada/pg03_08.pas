Program de_la_pagina_3_problema_8;
uses crt;
var a,i,k:integer;
    med:real;
    divizor:array[1..30000] of integer;

begin
 clrscr;
 writeln;
 writeln('Acesta este un program care daca ii veti introduce un numar a va a-');
 writeln('fisa media aritmetica si geometrica a divizorilor sai.');
 write('a=');readln(a);
 writeln;
 k:=1;
 for i:=1 to a do
  if a mod i = 0 then
                  begin
                   divizor[k]:=i;
                   k:=k+1;
                  end;
 med:=0;
 for i:=1 to k-1 do
  med:=med+divizor[i];
 med:=med/(k-1);
 writeln('Media aritmetica a divizorilor numarului ',a,' este ',med:3);
 med:=1;
 for i:=1 to k-1 do
  med:=med*divizor[i];
 med:=sqrt(med);
 writeln('Media geometrica a divizorilor numarului ',a,' este ',med:3);
 readkey;
end.