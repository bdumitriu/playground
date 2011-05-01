Program matrice4;
uses crt;
label 1;
type mat=array[1..20,1..20] of integer;
var a:mat;
    b:array[1..20] of boolean;
    i,j,m,n,x:integer;
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
i:=0;
while i<=m do
 begin
1: i:=i+1;
   for j:=1 to n do
    if a[i,j]<0 then
                 begin
                  b[i]:=true;
                  goto 1;
                 end
                else b[i]:=false;
 end;
write('Liniile care contin elemente negative sunt : ');
for i:=1 to m do
 if b[i]=true then write(i,' ');
writeln;
x:=0;
for i:=1 to m do
 for j:=1 to n do
  if b[i]=false then
                 if j=1 then
                         begin
                          x:=x+1;
                          writeln('Elementul [',x,',',j,'] este ',a[i,j]);
                         end
                        else
                         writeln('Elementul [',x,',',j,'] este ',a[i,j]);
readkey;
end.