Program de_calcul_a_mediei_aritmetice_a_elementelor_de_deasupra_diag_unei_m;
uses crt;
const m=3;
      n=3;
var a:array[1..m,1..n] of integer;
    i,j,p,x,y,z:integer;
    med:real;
begin
writeln('Acesta este un program care daca ii veti introduce elementele unei');
writeln('matrici va va calcula si afisa media numerelor aflate peste diago-');
writeln('nala principala');
writeln;
writeln('Introducti acum matricea');
for i:=1 to m do
for j:=1 to n do begin
                 write('matrice[',i,',',j,']=');readln(a[i,j]);
                 end;
x:=0;
y:=0;
z:=0;
for j:=2 to m do begin
x:=x+1;
for i:=1 to x do begin
y:=y+a[i,j];
z:=z+1;
end;
end;
med:=y/z;
writeln('media aritmetica a nr. este ',med);
while not keypressed do;
end.



