Program prelucrare_fisier_text;
uses crt;
var f:text;
    c:char;
    linie:string;
    i,q,j,t:integer;
begin
q:=1;
assign(f,'fis.txt');
reset(f);
read(f,linie);
for i:=1 to length(linie) do begin
if (linie[i]=' ') or (linie[i]=',') or (linie[i]='.') then q:=q+1;
end;
writeln('Numarul de inregistrari din fisierul g:\ELEV\CLS9E\BOGDAN\TURBSHIT\fis.txt este ',q);
write('Introduceti litera dorita : ');read(c);
if linie[1]=c then
begin
i:=1;
repeat
begin
write(linie[i]);
i:=i+1;
end;
until linie[i]=' ';
end;
i:=1;
repeat
begin
j:=j+1;
i:=i+1;
end;
until linie[i]=' ';
for i:=j to length(linie) do begin
if linie[i]=' ' then if linie[i+1]=c then begin
                                     t:=i;
                                     repeat
                                     begin
                                     write(linie[t]);
                                     t:=t+1;
                                     end;
                                     until linie[t]=' ';
                                     end;
end;
readkey;
end.