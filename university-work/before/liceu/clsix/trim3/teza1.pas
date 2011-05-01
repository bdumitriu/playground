Program teza1;
uses crt;
type persoana=record
              nume,prenume:string;
              varsta,inaltime:integer;
              end;
var elev,om:persoana;
    f:file of persoana;
    i,m:integer;
    c:char;
begin
repeat
writeln;
write('Introduceti nr de elevi [1 pana la 50] : ');read(m);
until (m<=50) and (m>0);
assign(f,'teza1.dat');
rewrite(f);
for i:=1 to m do
with elev do begin
 write('Numele : ');readln(nume);readln;
 write('Prenume : ');readln(prenume);
 write('Varsta : ');readln(varsta);
 write('Inaltime : ');readln(inaltime);
write(f,elev);
 end;
Writeln('Ce parte a problemei doriti sa executati  ? [a,b,c,d,e] ');readln(c);
case c of
'a':
begin
 reset(f);
 while not eof(f) do begin
 read(f,elev);
 with elev do begin
 writeln;
 writeln(nume);
 writeln(prenume);
 writeln(varsta);
 writeln(inaltime);
 end;
 end;
close(f);
end;
'b':
begin
 reset(f);
 read(f,om);i:=1;elev[i]:=om;
 while not eof(f) do begin
 read(f,om);i:=i+1;
 elev[i]:=om;
 end;
 for i:=1 to m-1 do begin
  if elev[i].nume>elev[i+1].nume then begin
                                 aux:=elev[i];
                                 elev[i]:=elev[i+1];
                                 elev[i+1]:=aux;
                                 end;
  end;
  for i:=1 to m do begin
  write(f,elev[i]);
  end;
end;
end;
readkey;
end.