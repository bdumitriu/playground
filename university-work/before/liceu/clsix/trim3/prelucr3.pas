Program reactualizare;
uses crt;
const n=50;
type elev=record
          nume:string;
          clasa:string;
          suma:longint;
          end;
var scolar:elev;
    f:file of elev;
    c,i,m:shortint;
    s:char;
    suma:longint;
    q,r:string;
begin
assign(f,'cec.dat');
reset(f);
writeln('Introduceti nr. de ordine al elevului a carui suma doriti s-o mo-');
write('dificati : ');readln(c);
write('Introduceti numele elevului : ');readln(q);
write('Introduceti clasa  elevului : ');readln(r);
write('Doriti sa extrageti sau sa depuneti mai multi bani [e/d] ? ');readln(s);
write('Introduceti suma : ');readln(suma);
seek(f,c);read(f,scolar);
if (q=scolar.nume) and (r=scolar.clasa) then
begin
if s='e' then begin
              scolar.suma:=scolar.suma-suma;
              write(f,scolar);
              end
         else begin
              scolar.suma:=scolar.suma+suma;
              write(f,scolar);
              end;
end
else writeln('Numarul de ordine nu corespunde cu datele ulterioare ale ele-');
     writeln('vului');
close(f);
readkey;
end.
















     ,
