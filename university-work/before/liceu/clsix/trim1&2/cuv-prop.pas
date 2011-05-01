Program nr_de_cuvinte_din_fiecare_propozitie;
uses crt;
var linie:string;
    i,c,p:integer;
begin
writeln('Acesta este un program care daca ii veti introduce un text de cu-');
writeln('vinte despartite intre ele printr-un singur spatiu sau printr-o');
writeln('virgula iar propozitiile despartite intre ele prin punct va va a-');
writeln('fisa cate cuvinte are fiecare propozitie si cate propozitii sunt');
writeln('in text');
write('Introduceti linia:');readln(linie);
c:=0;p:=0;
for i:=1 to length(linie) do begin
if (linie[i]=' ') or (linie[i]=',') then c:=c+1
else if linie[i]='.' then
                      begin
                      c:=c+1;
                      p:=p+1;
                      writeln('Nr. de cuvinte in propozitia ',p,' este ',c);
                      c:=0;
                      end;
end;
writeln('Nr. de propozitii este ',p);
while not keypressed do;
end.
