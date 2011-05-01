Program paritiile_unei_multimi;
uses crt;
var n:integer;
    p:array[1..100] of integer;

Procedure scrie;
var x,y,max:integer;
begin
 max:=0;
 for x:=1 to n do
  if max<p[x] then max:=p[x];
 for x:=1 to max do
  begin
   write('{');
   for y:=1 to n do
    if p[y]=x then write(y,' ');
   write('} ');
  end;
 writeln;
end;

Procedure part(i:integer);
var max,k,j:integer;
begin
 max:=0;
 for k:=1 to i-1 do
  if p[k]>max then max:=p[k];
 for j:=1 to max+1 do
  begin
   p[i]:=j;
   if i=n then scrie
          else part(i+1);
  end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de elemente din multime : ');readln(n);
 part(1);
 readkey;
end.