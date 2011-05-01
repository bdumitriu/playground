Program umplere_a_unei_matrici;
uses crt;
type sir=array[1..4] of shortint;
const x:sir=(-1,0,1,0);
      y:sir=(0,1,0,-1);
var mat:array[1..20,1..20] of integer;
    m,n,i,j,nr,l,ci,cj:integer;

Procedure umplere(i,j:byte);
var k:byte;
    ii,jj:shortint;
begin
 for k:=1 to 4 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
   if (ii in [1..m]) and (jj in [1..n]) then
    if mat[ii,jj]=0 then
                     begin
                      mat[ii,jj]:=1;
                      umplere(ii,jj);
                     end;
  end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de linii ale matricii : ');readln(m);
 write('Numarul de coloane ale matricii : ');readln(n);
 write('Cate elemente au valoarea 1 ? ');readln(nr);
 for i:=1 to m do
  for j:=1 to n do
   mat[i,j]:=0;
 for l:=1 to nr do
  begin
   write('Elementul ',l,' : ');
   readln(i,j);
   mat[i,j]:=1;
  end;
 write('Coordonatele elementului 0 initial : ');readln(ci,cj);
 umplere(ci,cj);
 for i:=1 to m do
  begin
   for j:=1 to n do
    write(mat[i,j]:2);
   writeln;
  end;
 readkey;
end.