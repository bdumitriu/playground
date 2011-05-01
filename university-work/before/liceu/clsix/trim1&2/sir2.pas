Program sir2;
uses crt;
const n=10;
type vect=array[1..n]of integer;
var i:integer;
    a,b:vect;
begin
writeln('Acesta este un program care daca ii veti introduce 10 numere va');
writeln('va afisa un alt sir de numere compus din sirul introdus de dumnea- ');
writeln('voastra scris de la coada la cap');
writeln('');
writeln('Introduceti cele zece numere');
writeln('');
for i:= 1 to n do begin
                  write('a[',i,']=');readln(a[i]);
                  end;
for i:= n downto 1 do begin
                      writeln('b[i]=',a[i])
                      end;
while not keypressed do;
end.