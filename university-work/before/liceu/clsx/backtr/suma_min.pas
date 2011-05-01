Program triunghi_cu_suma_minima;
uses crt;
type mat=array[1..4,1..4] of integer;
var i,j,m,sum,min:integer;
    a:mat;
    y,s:array[1..10] of integer;

Procedure suma;
var j:integer;
begin
 sum:=0;
 for j:=1 to m do
  sum:=sum+s[j];
 if min>sum then min:=sum;
end;

Procedure minim(i:integer);
var j:integer;
    test:boolean;
begin
 for j:=1 to i do
  begin
   s[i]:=a[i,j];
   y[i]:=j;
   test:=false;
   if i>1 then if (y[i]=y[i-1]) or (y[i]-1=y[i-1]) then test:=true;
   if i=1 then test:=true;
   if test then
    if i=m then suma
           else minim(i+1);
  end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de linii : ');
 readln(m);
 for i:=1 to m do
  for j:=1 to m do
   a[i,j]:=0;
 for i:=1 to m do
  for j:=1 to i do
   begin
    write('Elementul ',j,' : ');
    readln(a[i,j]);
   end;
 min:=maxint;
 minim(1);
 writeln('Suma minima este : ',min,'.');
 readkey;
end.