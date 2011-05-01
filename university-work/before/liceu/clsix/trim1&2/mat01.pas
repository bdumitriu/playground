Program matrice1;
uses crt;
type mat=array[1..20,1..20] of integer;
var a:mat;
    i,j,m,n,max,min:integer;
begin
clrscr;
writeln;
write('Cate linii are matricea ? ');readln(m);
write('Cate coloane are matricea ? ');readln(n);
writeln('Introduceti matricea. Atentie! Fara elementul 0!!! ');
for i:=1 to m do
 for j:=1 to n do
  begin
   write('Elementul [',i,',',j,'] : ');
   readln(a[i,j]);
  end;
max:=a[1,1];
min:=a[1,1];
for i:=1 to m do
 for j:=1 to n do
  begin
   if a[i,j]>max then max:=a[i,j];
   if a[i,j]<min then min:=a[i,j];
  end;
writeln('Raportul dintre cel mai mare element al matricei si cel mai mic');
writeln('element al ei este :',max/min);
readkey;
end.