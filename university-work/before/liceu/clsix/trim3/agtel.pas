Program agenda_telefonica;
uses crt;
const n=100;
type om=record
        nume:string;
        nr_tel:longint;
        end;
var f:file of om;
    persoana:array[1..n] of om;
    m,i:integer;
    aux:om;
procedure creare ;
var o:om;
begin
rewrite(f);
repeat
write('Introduceti nr. de persoane pe care doriti sa le introduceti : ');
readln(m);
until m<=n;
for i:=1 to m do
with o do begin
write('Numele ');readln(nume);
write('Numarul de telefon ');readln(nr_tel);
write(f,o);
end;
close(f);
end; {aici se incheie procedura 1}
procedure sortare;
var o:om;
    j:integer;
begin
reset(f);
read(f,o);i:=1;persoana[i]:=o;
while not eof(f) do begin
read(f,o);i:=i+1;
persoana[i]:=o;
end;
assign(f,'agsortat.txt');
rewrite(f);
for i:=1 to m-1 do
  for j:=i+1 to m do
 begin
if persoana[i].nume>persoana[j].nume then begin
                          aux:=persoana[i];
                          persoana[i]:=persoana[j];
                          persoana[j]:=aux;
                          end;
end;
for i:=1 to m do begin
write(f,persoana[i]);
end;
end; {aici se incheie procedura 2}
procedure listare;
var o:om;
begin
reset(f);
repeat
begin
read(f,o);
writeln(o.nume);
writeln(o.nr_tel);
end;
until eof(f);
close(f);
end;
begin
assign(f,'agendat.txt');
creare;
sortare;
listare;
readkey;
end.

