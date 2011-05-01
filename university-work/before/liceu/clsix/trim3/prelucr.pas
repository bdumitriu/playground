Program de_prelucrare_a_fisierului_cec;
uses crt;
type persoana=record
              nume:string;
              clasa:string;
              suma:longint;
              end;
var f:file of persoana;
    s:longint;
    elev:persoana;
begin
assign(f,'cec.dat');
reset(f);
s:=0;
while not eof(f) do begin
read(f,elev);
if elev.suma>s then s:=elev.suma;
end;
reset(f);
writeln('Suma maxima este ',s);
writeln('Persoanele cu suma maxima sunt: ');
while eof(f) do begin
read(f,elev);
if elev.suma=s then writeln(elev.nume);
end;
close(f);
while not keypressed do;
end.
