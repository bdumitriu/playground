Program afisare_a_persoanelor_nascute_in_martie_intre_anii_1950_si_1980;
uses crt;
const n=5;
type om=record
        nume:string[20];
        adresa:string;
        nrtel:longint;
        datan:record
              ziua:1..31;
              luna:1..12;
              anul:integer;
              end;
        end;
var a:array[1..n] of om;
    i:integer;
begin
writeln('Acesta este un program care daca ii veti introduce datele personale');
writeln('a 5 persoane va va afisa pe acelea care sunt nascute in martie in-');
writeln('tre anii 1950 si 1980');
for i:=1 to n do
with a[i] do begin
write('Numele persoanei[',i,'] este ');readln(nume);
write('Adresa persoanei[',i,'] este ');readln(adresa);
write('Numarul de telefon al persoanei[',i,'] este ');readln(nrtel);
writeln('Data nasterii persoanei[',i,'] este');
write('Ziua nasterii este(a se scrie doar cifra resp.)');readln(datan.ziua);
write('Luna nasterii este (1,2,3,4,5,6,7,8,9,10,11,12');readln(datan.luna);
write('Anul nasterii este ');readln(datan.anul);
end;
writeln('Acestea sunt persoanele care sunt nascute in martie intre 50-80');
for i:=1 to n do
with a[i] do begin
if datan.luna=3 then begin
if (datan.anul>=1950) and(datan.anul<=1980) then begin
writeln(nume);
writeln(adresa);
writeln(nrtel);
end;end;end;
while not keypressed do;
end.
