Program OJ_88;
uses crt;
var n,i,j:byte;
    mat,matfin:array[1..100,1..100] of integer;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce o matrice o va');
 writeln('roti pe aceasta cu 180 de grade si o va afisa in forma finala.');
 writeln;
 textcolor(lightblue);
 write('Introduceti numarul de linii si coloane ale matricii : ');readln(n);
 for i:=1 to n do
  for j:=1 to n do
   begin
    write('Elementul ',i,',',j,' : ');
    readln(mat[i,j]);
   end;
 textcolor(yellow);
 writeln;
 if n mod 2 = 1 then
  begin
   for i:=1 to n do
    for j:=1 to n div 2 do
     matfin[i,j]:=mat[i,n-j+1];
   for i:=1 to n do
    for j:=(n div 2)+1 to n do
     matfin[i,j]:=mat[i,n-j+1];
  end;
 if n mod 2 = 0 then
  begin
   for i:=1 to n do
    for j:=1 to n div 2 do
     matfin[i,j]:=mat[i,n-j+1];
   for i:=1 to n do
    for j:=(n div 2)+1 to n do
     matfin[i,j]:=mat[i,n-j+1];
  end;
 for i:=1 to n do
  begin
   for j:=1 to n do
    write(matfin[i,j]:3);
   writeln;
  end;
 readkey;
end.