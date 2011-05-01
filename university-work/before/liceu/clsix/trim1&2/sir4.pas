Program NumarInSir;
uses crt;
const Max=20;
type vect=array[1..Max] of integer;
var i,a,n,k:integer;
    x:vect;
begin
writeln('Acesta este un program care daca ii veti introduce un sir de nume-');
writeln('re (maixm 20-numarul exact introducandu-l dumneavoastra) si un alt');
writeln('numar "a" va verifica daca numarul introdus de dumneavoastra se afla');
writeln('in sir si in caz ca se afla va va spune locul ocupat de el in sir');
repeat
writeln('Introduceti va rog numarul de numere al sirului (maxim 20) ');
read(n)
until n<=20;
writeln;
writeln('Acum introduceti va rog sirul de numere');
for i:=1 to n do begin
                 write('x[',i,']=');
                 read(x[i]);
                 end;
writeln;
writeln('Si acum introduceti numarul "a"');
write('a= ');read(a);
for i:=1 to n do begin
                 if a=x[i] then
                 writeln('Numarul este in sir si se afla pe pozitia ',i)
                 else k:=k+1;
                 end;
if k=n then  begin
             writeln('Numarul introdus de dumneavoastra nu face parte din');
             writeln('sir');
             end;
while not keypressed do;
readln;
end.
