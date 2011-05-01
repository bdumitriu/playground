Program matrice3;
uses crt;
label 1;
type mat=array[1..20,1..20] of integer;
var a:mat;
    i,j,m,n,min,x,y:integer;
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
 for j:=1 to n do
  begin
   if a[i,j] >=0 then min:=a[i,j];
   goto 1;
  end;
1:for i:=1 to m do
 for j:=1 to n do
  if a[i,j]>=0 then if min>=a[i,j] then
                                   begin
                                    x:=i;
                                    y:=j;
                                   end;
writeln('Linia cu cel mai mic element este linia ',x);
writeln('Coloana cu cel mai mic element este coloana ',y);
readkey;
end.
