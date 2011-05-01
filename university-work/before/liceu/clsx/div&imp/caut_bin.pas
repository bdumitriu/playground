Program cautare_binara;
uses crt;
var x:array[1..100] of integer;
    n,a,i:integer;
    q:byte;

Function cauta(s,d:byte):byte;
var r:byte;
begin
 cauta:=0;
 while s<=d do
  begin
   if a=x[(s+d) div 2] then
                        begin
                         cauta:=(s+d) div 2;
                         s:=5;d:=4;
                        end
                       else if a < x[(s+d) div 2] then d:=(s+d) div 2 - 1
                                                  else s:=(s+d) div 2 + 1;
  end;
end;


begin
 clrscr;
 writeln;
 write('Numarul de elemente din sir : ');readln(n);
 writeln('Introduceti sirul.Atentie, trebuie sa fie ordonat crescator.');
 for i:=1 to n do
  begin
   write('Elementul ',i,' : ');
   readln(x[i]);
  end;
 write('Numarul ce trebuie cautat in sir : ');readln(a);
 q:=cauta(1,n);
 if q<>0 then writeln('Numarul este pe pozitia ',q,'.')
         else writeln('Numarul nu se afla in sir.');
 readkey;
end.