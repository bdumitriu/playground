Program matrice7;
uses crt;
type mat=array[1..20,1..20] of integer;
var a:mat;
    i,j,m,n:integer;
    min:array[1..20] of integer;
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
for i:=1 to m do
 begin
  min[i]:=a[i,1];
  for j:=2 to n do
   if min[i]>a[i,j] then min[i]:=a[i,j];
 end;
for i:=1 to m do
 for j:=1 to n do
  a[i,j]:=a[i,j]-min[i];
for i:=1 to m do
 for j:=1 to n do
  writeln('Elementul [',i,',',j,'] este : ',a[i,j]);
readkey;
end.
