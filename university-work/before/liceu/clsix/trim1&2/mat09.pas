Program matrice9;
uses crt;
type mat=array[1..20,1..20] of real;
var a:mat;
    i,j,m,n:integer;
    med,numarator,numitor:array[1..20] of real;
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
 numitor[i]:=0;
for i:=1 to m do
 begin
  numarator[i]:=0;
  for j:=1 to n do
   if a[i,j]>=0 then
                 begin
                  numarator[i]:=numarator[i]+a[i,j];
                  numitor[i]:=numitor[i]+1;
                 end;
 end;
for i:=1 to m do
 if numarator[i]>0 then med[i]:=numarator[i]/numitor[i]
                   else med[i]:=1;
for i:=1 to m do
 for j:=1 to n do
  a[i,j]:=a[i,j]/med[i];
for i:=1 to m do
 for j:=1 to n do
  writeln('Elementul [',i,',',j,'] este : ',a[i,j]);
readkey;
end.