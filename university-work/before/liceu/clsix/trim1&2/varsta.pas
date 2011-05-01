Program varsta;
uses crt;
var V,Ziua,Luna,Anul,Ziuan,lunan,anuln:integer;
begin
writeln('Acesta este un program care daca ii veti introduce data actuala');
writeln('(ZZ/LL/AAAA) si data nasterii unei persoane (ZZ/LL/AAAA) va va a-');
writeln('varsta acesteia in ani');
writeln('Introduceti data actuala');
write('Ziua :  ');read(Ziua);
write('Luna :  ');read(Luna);
write('Anul :  ');read(Anul);
writeln('Introduceti data de nastere a persoanei a carei varsta doriti sa o');
writeln('aflati');
write('Ziua nasterii :  ');read(Ziuan);
write('Luna nasterii :  ');read(Lunan);
write('Anul nasterii :  ');read(Anuln);
if Lunan>Luna then V:=Anul-Anuln-1
              else if Lunan<Luna then V:=Anul-Anuln
                   else if Lunan=Luna then begin
                                           if Ziuan>Ziua
                                           then
                                           V:=Anul-Anuln-1
                                           else if Ziuan<Ziua then
                                                           V:=Anul-Anuln
                                           else if Ziuan=Ziua then
                                                        V:=Anul-Anuln;
                                           end;
writeln('Varsta persoanei a carei data de nastere a fost introdusa de dum-');
write('neavoastra este de ',V);write(' ani');
while not keypressed do;
readln;
end.