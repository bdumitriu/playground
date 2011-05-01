Program Modul;
Uses Crt;
var x:real;
begin
writeln ('Acesta este un program pentru aflarea modulului dintr-un numar');
writeln ('Introduceti numarul');
write ('x=');read(x);
if x>=0 then write ('|x|=',x)
        else write ('|x|=',-x);
while not keypressed do;
readln;
end.