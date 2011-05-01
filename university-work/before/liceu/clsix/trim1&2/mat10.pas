Program matrice10;
uses crt;
type mat=array[1..20,1..20] of integer;
var a,b,c:mat;
    i,j,m,n:integer;
begin
clrscr;
writeln;
write('Cate linii au matricile ? ');readln(m);
write('Cate coloane are matricile ? ');readln(n);
writeln('Introduceti matricea 1.');
for i:=1 to m do
 for j:=1 to n do
  begin
   write('Elementul [',i,',',j,'] : ');
   readln(a[i,j]);
  end;
writeln('Introduceti matricea 2.');
for i:=1 to m do
 for j:=1 to n do
  begin
   write('Elementul [',i,',',j,'] : ');
   readln(b[i,j]);
  end;
for i:=1 to m do
 for j:=1 to n do
  c[i,j]:=a[i,j]+b[i,j];
for i:=1 to m do
 for j:=1 to n do
  writeln('Elementul sumei [',i,',',j,'] este : ',c[i,j]);
readkey;
end.