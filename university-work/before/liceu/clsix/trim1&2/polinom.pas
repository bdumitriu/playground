Program polinom;
uses crt;
const s=20;
var a:array[1..s] of integer;
    x,y,z:longint;
    i,n:integer;
begin
writeln('Acesta este un program care daca ii veti introduce coeficientii ');
writeln('unui polinom de grad n(introdus de dumneavoastra) va va calcula');
writeln('valoarea pt. un x dat');
repeat
write('Introduceti gradul polinomului (n<20) :');read(n);
until  (n>1) and (n<20);
writeln;
writeln('Acum introduceti va rog coeficientii');
for i:=0 to n do begin
                 write('a[',i,']=');read(a[i]);
                 end;
writeln;
write('Acum introduceti valoarea x: ');read(x);
z:=x;
y:=0;
for i:=n downto 1 do begin
y:=y+a[i-1]*z;
z:=z*x;
end;
writeln('Valoarea este ',y+a[n]);
while not keypressed do;
readln;
end.






