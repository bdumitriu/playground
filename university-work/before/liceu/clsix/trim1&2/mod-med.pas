Program modul;
uses crt;
var a,b,c,ma:real;
begin
clrscr;
writeln;
writeln('Acesta este un program care daca veti introduce numerele reale');
writeln('a,b si c va tipari valoarea absoluta a fiecare din cele trei');
writeln('numere iar apoi media aritmetica a modulelor lor');
writeln('Introduceti cele trei numere');
write('a=');read(a);
write('b=');read(b);
write('c=');read(c);
ma:=(abs(a)+abs(b)+abs(c))/3;
writeln('|a|=',abs(a):10:2);
writeln('|b|=',abs(b):10:2);
writeln('|c|=',abs(c):10:2);
writeln('media aritmetica a celor trei module=',ma);
readkey;
readln;
end.
