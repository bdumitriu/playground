Program de_scriere_a_unui_numar_invers;
uses crt;
var n:integer;

Procedure scriere_inversa(n:integer);
begin
 write(n mod 10);
 if trunc(n/10)>=1 then scriere_inversa(trunc(n/10));
end;

begin
 clrscr;
 writeln;
 writeln('Acesta este un program care daca ii veti introduce un numar vi-l');
 writeln('va scrie cu cifrele inversate.');
 write('Numarul este : ');
 readln(n);
 scriere_inversa(n);
 readkey;
end.