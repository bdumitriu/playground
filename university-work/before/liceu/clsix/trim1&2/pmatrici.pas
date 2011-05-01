Program de_inmultire_a_doua_matrici;
uses crt;
type matrice1=array[1..10,1..10] of integer;
     matrice2=array[1..10,1..10] of integer;
     matrice3=array[1..10,1..10] of integer;
var mat1:matrice1;
    mat2:matrice2;
    mat3:matrice3;
    i,j,l,m,n,k:integer;
begin
 clrscr;
 writeln;
 writeln('Acesta este un program care daca ii veti introduce elementele a');
 writeln('doua matrici va va afisa matricea produs a acestor doua matrici');
 write(' Numarul de linii a primei matrici : ');
 readln(n);
 write(' Numarul de coloane a primei matrici(si implicit nr de linii a celei de-a doua): ');
 readln(m);
 write(' Numarul de coloane a celei de-a doua matrici : ');
 readln(k);
 for i:=1 to n do
  for j:=1 to m do
   begin
    write('Matricea 1[',i,',',j,']=');
    readln(mat1[i,j]);
   end;
 writeln;
 for j:=1 to m do
  for l:=1 to k do
   begin
    write('Matricea 2[',j,',',l,']=');
    readln(mat2[j,l]);
   end;
 for i:=1 to n do
  for l:=1 to k do
   begin
    mat3[i,l]:=0;
    for j:=1 to m do
     mat3[i,l]:=mat3[i,l]+mat1[i,j]*mat2[j,l];
   end;
 writeln;
 for i:=1 to n do
  for l:=1 to k do
   writeln('Matricea produs[',i,',',l,']= ',mat3[i,l]);
 readkey;
end.
