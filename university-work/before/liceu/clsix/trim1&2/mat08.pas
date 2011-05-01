Program matrice8;
uses crt;
label 1,2;
type mat=array[1..20,1..20] of integer;
var a:mat;
    x:array[1..20] of integer;
    i,j,m,n,y:integer;
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
i:=0;y:=1;
while i<=m do
 begin
1: i:=i+1;
   for j:=1 to n do
    if a[i,j]=0 then
                 begin
                  x[y]:=i;
                  y:=y+1;
                  if i<>m then goto 1
                          else goto 2;
                 end;
 end;
2: write('Indicii liniilor ce contin valori nule sunt : ');
for i:=1 to y-1 do
 write(x[i],' ');
readkey;
end.