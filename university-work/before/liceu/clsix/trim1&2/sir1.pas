Program sir1;
uses crt;
const n=5;
type vect=array[1..n]of integer;
var i,k:integer;
    a,b,c:vect;
    r:real;
begin
writeln('Acesta este un program care daca ii veti introduce 10 numere astfel:');
writeln('primele 5 in sirul "a" si urmatoarele 5 in sirul "b" va va afisa:');
writeln('');
writeln('1.un sir c[k] care va avea primele 5 numere cele');
writeln('din sirul a iar urmatoarele numere cele din sirul b daca veti ti-');
writeln('pari "1" cand va apare scris "r="');
writeln('');
writeln('2.un sir c[k] care va afisa numerele sale in ordinea urmatoarele ');
writeln('numere a[1],b[1],a[2],b[2],a[3],b[3],a[4],b[4],a[5],b[5] daca veti');
writeln('tipari "2" cand va apare scris "r="');
writeln('');
writeln('                         ATENTIE');
writeln('Programul nu lucreaza decat pt. valorile 1 si 2 ale lui r!');
writeln('Introduceti va rog alegerea dumneavoastra 1 sau 2');
writeln('');
write('r=');read(r);
writeln('');
if r=1 then begin
for i:=1 to n do begin
                 write('a[',i,']=');readln(a[i]);
                 end;
writeln('');
for i:=1 to n do begin
                 write('b[',i,']=');readln(b[i]);
                 end;
writeln('');
for i:=1 to n do begin
                 writeln('c[k]=',a[i]);
                 end;
for i:=1 to n do begin
                 writeln('c[k]=',b[i]);
                 end;
            end
       else if r=2 then begin
                        for i:=1 to n do begin
                                         write('a[',i,']=');read(a[i]);
                                         end;
                        writeln('');
                        for i:=1 to n do begin
                                         write('b[',i,']=');read(b[i]);
                                         end;
                        writeln('');
                        for i:=1 to n do begin
                                         writeln('c[k]=',a[i]);
                                         writeln('c[k]=',b[i]);
                                         end;
                        end
                   else begin
writeln('Acest program este facut doar pt. oamenii care citesc instructiu-');
writeln('nile de la inceputul programului si care le respecta.Acolo se spu-');
writeln('nea ca programul lucreaza pt. valorile 1 si 2 ale lui r =>pt. orice');
writeln('alta valoare nu face nimic deci incepeti de la inceput programul');
writeln('si introduceti valoarea 1 sau 2 pt. r daca vreti sa va mearga.');
writeln('SUCCES!!! SI INVATATI SA CITITI!!!');
end;
while not keypressed do;
readln;
end.










