Program de_ordonat_elementele_de_pe_diagonala_principala_a_unei_matrici;
uses crt;
const m=3;
      n=3;
      p=3;
type mat=array[1..m,1..n] of integer;
var a:mat;
    i,j,aux:integer;
    c:boolean;
begin
writeln('Acesta este un program care daca ii veti introduce o matrice va va');
writeln('afisa elementele de pe diagonala principala a matricii in ordine ');
writeln('crescatoare');
writeln('Introduceti matricea:');
for i:=1 to m do
 for j:=1 to n do begin
                  write('matrice[',i,',',j,']=');readln(a[i,j]);
                  end;
repeat begin
c:=true;
for i:=1 to m-1 do
 for j:=1 to n-1 do begin
if i=j then begin
if a[i,j]>a[i+1,j+1] then
begin
c:=false;
aux:=a[i,j];
a[i,j]:=a[i+1,j+1];
a[i+1,j+1]:=aux;
end; end;
end;
end;
until c;
for i:=1 to m do
for j:=1 to n do begin
if i=j then
writeln(a[i,j]);
end;
while not keypressed do;
end.
