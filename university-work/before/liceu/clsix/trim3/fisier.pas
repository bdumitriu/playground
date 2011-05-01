Program CEC;
uses crt;
const n=50;
type elev=record
          nume:string;
          clasa:string;
          suma:longint;
          end;
var scolar:elev;
    f:file of elev;
    i,m:shortint;
    c:string;
    S:longint;
begin
clrscr;
writeln;
repeat
write('Introduceti nr. de elevi (maximum 50) : ');read(m);
until m<=50;
assign(f,'cec.dat');
rewrite(f);
for i:=1 to m do
with scolar do begin
               write('Numele elevului ',i,' : ');readln(nume);
               write('Clasa in care este elevul ',i,' : ');readln(clasa);
               write('Suma la CEC a elevului ',i,' : ');readln(suma);
               write(f,scolar);
               end;
reset(f);
read(f,scolar);
S:=scolar.suma;
c:=scolar.clasa;
writeln('Clasa ',scolar.clasa);
writeln(scolar.nume,' ',scolar.suma);
while not eof(f) do begin
read(f,scolar);
if c=scolar.clasa then begin
writeln(scolar.nume,' ',scolar.suma);
S:=S+scolar.suma;
end
else begin
writeln('Suma depusa la CEC de clasa a ',c,' este ',S);
S:=scolar.suma;
c:=scolar.clasa;
writeln('Clasa ',scolar.clasa);
writeln(scolar.nume,' ',scolar.suma);
end;
end;
writeln('Suma depusa la CEC de clasa a ',c,' este ',S);
close(f);
readkey;
end.

