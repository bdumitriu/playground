Program primdiv;
uses crt;
var k,i,a:longint;
begin
writeln('Acesta este un program care daca ii veti introduce un numar va');
writeln('verifica daca acest numar e prim iar daca nu e va afisa divizorii');
writeln('sai si numarul acestora');
write('a=');read(a);
k:=0;
for i:=2 to (a div 2) do begin
                         if a/i=(a div i) then begin
                                               k:=k+1;
                                               writeln(i);
                                               end;
                         end;
if k<>0 then writeln('numarul divizorilor e  ',k)
        else writeln('numarul e prim');
while not keypressed do;
readln;
end.