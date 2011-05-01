Program elevi_cu_fisier;
uses crt;
const n=50;
type elev=record
          nume:string;
          varsta:byte;
          end;
var scolar:array[1..n] of elev;
    f:file of elev;
    i,j:shortint;
    m:string;
    q:integer;
begin
assign(f,'elevi2.dat');
rewrite(f);
i:=1;
q:=0;
repeat
write('Numele elevului[',i,']:');readln(scolar[i].nume);
write('Varsta elevului[',i,']:');readln(scolar[i].varsta);
i:=i+1;
write('Doriti sa mai introduceti datele despre un elev [da,nu] ?');readln(m);
q:=q+1;
until m='nu';
for j:=1 to i do begin
                 write(f,scolar[j]);
                 end;
close (f);
writeln('Nr. de componente este ',q);
readkey;
end.
