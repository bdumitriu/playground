program de_interschimbare_a_cifrelor_unui_sir;
uses crt;
const n=10;
var a:array[1..n] of integer;
    i,p:integer;
begin
writeln('Acesta este un program care daca ii veti introduce un sir de 10 nr.');
writeln('vi- le va afisa pe acestea in ordine inversa');
for i:=1 to n do begin
write('a[',i,']=');readln(a[i]);
end;
writeln;
for i:=n downto 1 do begin
writeln('a[i]=',a[i]);
end;
while not keypressed do;
end.