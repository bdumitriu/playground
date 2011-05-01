Program matrice6;
uses crt;
type mat=array[1..20,1..20] of integer;
var a:mat;
    i,j,m,s:integer;
begin
clrscr;
writeln;
write('Cate linii si coloane are matricea (e patratica) ? ');readln(m);
writeln('Introduceti matricea.');
for i:=1 to m do
 for j:=1 to m do
  begin
   write('Elementul [',i,',',j,'] : ');
   readln(a[i,j]);
  end;
s:=0;
for i:=1 to m do
 for j:=1 to m do
  if i=j then s:=s+a[i,j];
writeln('Suma elementelor de pe coloana principala este ',s);
readkey;
end.
