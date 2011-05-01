Program de_aflare_a_sumei_cifrelor_pare_a_unui_numar_cu_functie_recursiva;
uses crt;
var n:integer;
    s:byte;

Function suma(n:integer):byte;
begin
 if trunc(n/10)<1 then if (n mod 10) mod 2 = 0 then
  suma:=n
                                               else
  suma:=0;
 if trunc(n/10)>=1 then if (n mod 10) mod 2 = 0 then
  suma:=suma(trunc(n/10))+n mod 10
                                                else
  suma:=suma(trunc(n/10));
end;

begin
 clrscr;
 writeln;
 writeln('Acesta este un program care daca ii veti introduce un numar va va');
 writeln('calcula si afisa suma cifrelor sale pare');
 write('Numarul este : ');
 readln(n);
 s:=suma(n);
 writeln('Suma cifrelor pare a numarului ',n,' este : ',s);
 readkey;
end.