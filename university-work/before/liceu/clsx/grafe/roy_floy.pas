Program algoritmul_Roy_Floyd;
uses crt;
var a:array[1..5 ,1..5 ] of integer;
    k,i,j,m,n,x,y:integer;

begin
 clrscr;
 writeln;
 write(' Introduceti numarul de varfuri : ');readln(n);
 write(' Introduceti numarul de muchii : ');readln(m);
 for i:=1 to n do
  for j:=1 to n do
   a[i,j]:=0;
 for i:=1 to m do
  begin
   write(' Arcul ',i,' : ');readln(x,y);
   write(' Lungimea arcului ',i,' : ');readln(k);
   a[x,y]:=k;
  end;
 for i:=1 to n do
  for j:=1 to n do
   if (i<>j) and (a[i,j]=0) then a[i,j]:=1000;
 for k:=1 to n do
  for i:=1 to n do
   for j:=1 to n do
    if a[i,j]<=a[i,k]+a[k,j] then a[i,j]:=a[i,j]
                             else a[i,j]:=a[i,k]+a[k,j];
 for i:=1 to n do
  begin
   for j:=1 to n do
    write(a[i,j],' ');
   writeln;
  end;
 readkey;
end.