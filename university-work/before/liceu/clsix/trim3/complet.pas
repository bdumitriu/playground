Program de_completare;
uses crt;
type persoana=record
     nume:string;
     clasa:string;
     suma:longint;
     end;
var f:file of persoana;
    elev:array[1..50] of persoana;
    i,m:integer;
begin
assign(f,'cec.dat');
reset(f);
seek(f,filesize(f));
repeat
write('Introduceti nr. de elevi pe care mai doriti sa-i introduceti :');
readln(m);
until m<=50;
for i:=1 to m do begin
write('Numele persoanei[',i,'] este ');readln(elev[i].nume);
write('Clasa in care este persoana[',i,'] :');readln(elev[i].clasa);
write('Suma pe care o are persoana[',i,'] la CEC ');readln(elev[i].suma);
write(f,elev[i]);
end;
readkey;
end.