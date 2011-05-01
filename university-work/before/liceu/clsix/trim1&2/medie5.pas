Program deop;
uses crt;
const n=5;
type vect=array[1..n] of integer;
var a:vect;
    i:integer;
begin
writeln('Acesta este un program care daca ii veti introduce cinci numere');
writeln('va va afisa urmatoarea lista a[1];m(a[1] si a[2]);a[2];m(a[2] si');
writeln('a[3]);a[3];m(a[3] si a[4]);a[4];m(a[4] si a[5]);a[5],unde a[1]-[5]');
writeln('sunt numerele introduse de dumneavoastra in ordinea in care au fost');
writeln('introduse,m(a[1] si a[2]) este media dintre numerle a[1] si a[2],');
writeln('m(a[2] si a[3]) este media numerelor a[2] si a[3] si asa mai departe');
writeln('');
writeln('Inroduceti va rog numerele');
for i:=1 to n do begin 
                 write('a[',i,']=');read(a[i]);
                 end;
for i:=1 to n do begin
                 writeln(a[i]);
                 writeln(a[i]/2+a[i+1]/2);
                 end;
while not keypressed do;
readln;
end.