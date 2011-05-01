Program matrice2;
uses crt;
type natural=0..maxint;
     mat=array[1..20,1..20] of natural;
var a:mat;
    b:array[1..20] of natural;
    i,j,m,n:integer;
begin
clrscr;
writeln;
write('Cate linii are matricea ? ');readln(m);
write('Cate coloane are matricea ? ');readln(n);
writeln('Introduceti matricea.');
for i:=1 to m do
 for j:=1 to n do
  begin
   write('Elementul [',i,',',j,'] : ');
   readln(a[i,j]);
  end;
for i:=1 to n do
 b[i]:=0;
for j:=1 to n do
 for i:=1 to m do
   if b[j]<a[i,j] then b[j]:=a[i,j];
for i:=1 to n do
 writeln('Elementul maxim din coloana ',i,' este ',b[i]);
readkey;
end.