Program matrice_circulara;
uses crt;
var a,b,c,d,i,j,m,n:shortint;
    mat:array[1..10,1..10] of integer;
    s:array[1..100] of integer;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce o matrice pa-');
 writeln('tratica va va afisa matricea parcursa sub forma de melc.');
 writeln;
 textcolor(lightblue);
 write('Introduceti numarul de linni si coloane ale matricii : ');readln(n);
 for i:=1 to n do
  for j:=1 to n do
   begin
    write('Elementul ',i,',',j,' : ');
    readln(mat[i,j]);
   end;
 writeln;
 textcolor(yellow);
 a:=1;
 b:=n;
 c:=2;
 d:=n;
 m:=0;
 repeat
  for j:=a to b do
   begin
    m:=m+1;
    s[m]:=mat[c-1,j];
   end;
  b:=b-1;
  for i:=c to d do
   begin
    m:=m+1;
    s[m]:=mat[i,b+1];
   end;
  d:=d-1;
  for j:=b downto a do
   begin
    m:=m+1;
    s[m]:=mat[d+1,j];
   end;
  a:=a+1;
  for i:=d downto c do
   begin
    m:=m+1;
    s[m]:=mat[i,a-1];
   end;
  c:=c+1;
 until m=n*n;
 for i:=1 to n*n do
  write(s[i],' ');
 readkey;
end.