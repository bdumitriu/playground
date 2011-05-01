Program nrprim;{Daca k=0 atunci nr. e prim;iar daca k=1 atunci nr. nu e prim}
uses crt;
var n,i,k:integer;
    a:string[25];
begin
write('  INTRODUCETI CODUL DE ACCES : ');read(a);
if a='cd radio' then begin;
writeln('Acesta este un program prin care se afla daca un numar n e prim');
writeln('sau nu');
writeln('Introduceti numarul n');
write('n=');read(n);
k:=0;
for i:=2 to n-1 do begin;
                        if n/i=(n div i) then k:=1
                                         else k:=k;
                        end;
if k=0 then writeln('numarul n e prim')
       else if k=1 then writeln('numarul n nu e prim');
        end
 else writeln('Sorry, da` fara d-ala(=cod) nu se poate deci bye-bye');
while not keypressed do;
readln;
end.
