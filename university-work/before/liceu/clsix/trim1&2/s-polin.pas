Program suma_a_doua_polinoame;
uses crt;
const n=10;
type coef=array[1..n] of integer;
var a,b:coef;
    i,j,p,q:integer;
begin
writeln('Acesta este un program care daca ii veti introduce 2 polinoame va');
writeln('afisa coeficientii lui x din polinomul suma');
writeln('Introduceti polinomul 1:');
write('Gradul polinomului 1 este ');read(p);
for i:=1 to p+1 do begin
write('coeficientul lui x este ');read(a[i]);
end;
writeln('Introduceti polinomul 2:');
write('Gradul polinomului 2 este ');read(q);
for j:=1 to q+1 do begin
write('coeficientul lui x este ');read(b[j]);
end;
if p>q then begin
for i:=1 to p-q do begin
writeln('Coeficientul lui x la puterea ',p+1-i,' este ',a[i]);
end;
for i:=p-q to p+1 do
for j:=1 to q+1 do begin
if i=j+p-q then
writeln('Coeficientul lui x la puterea ',p+1-i,' este ',a[i]+b[j]);
end;
end
else begin
for j:=1 to q-p do begin
writeln('Coeficientul lui x la puterea ',q+1-j,' este ',b[j]);
end;
for j:=1 to q+1 do
for i:=1 to p+1 do begin
if j=i+q-p then
writeln('Coeficientul lui x la puterea ',q+1-j,' este ',a[i]+b[j]);
end;
end;
while not keypressed do;
end.
