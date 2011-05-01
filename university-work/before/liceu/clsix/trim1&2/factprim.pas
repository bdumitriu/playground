Program descompunere_numar_in_factori_primi;
uses crt;
var a,n:longint;
    i:2..maxlongint;
begin
writeln('Acesta este un program care daca ii vati introduce un numar a va');
writeln('afisa toti divizorii respectivului numar');
writeln('Introduceti va rog numarul a');
write('a=');read(a);
writeln('Acestia sunt factorii primi in care se descompune numarul introdus');
writeln('dumneavostra');
i:=2;
while i<a do begin
             repeat
             begin
             if (a div i)=a/i then begin
                                   writeln(i);
                                   a:=a div i;
                                   i:=1+1;
                                   end
                              else i:=i+1;
             end;
             until(a div i)<>(a/i);
             end;
while not keypressed do;
readln;
end.