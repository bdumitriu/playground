Program matrice5;
uses crt;
type mat=array[1..20,1..20] of integer;
var x:mat;
    a,b,i,j,m,n,y:integer;
    v:array[1..1000] of integer;
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
   readln(x[i,j]);
  end;
write('Introduceti extremitatea mica a intervalului : ');readln(a);
write('Introduceti extremitatea mare a intervalului : ');readln(b);
y:=1;
for i:=1 to m do
 for j:=1 to n do
  if (x[i,j]>=a) and(x[i,j]<=b) then
                                 begin
                                  v[y]:=x[i,j];
                                  y:=y+1;
                                 end;
for i:=1 to y-1 do
 writeln('Elementul ',i,' al vectorului este : ',v[i]);
readkey;
end.