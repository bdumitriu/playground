program prim2;
uses crt;
var a,i,j,k,n:integer;
begin
writeln('Acestea sunt toate numerele prime de la 1 la un numar oarecare');
writeln('introdus de dumneavoastra');
writeln('');
writeln('Introduceti va rog numarul');
write('a=');read(a); 
n:=0;
for i:=1 to a do begin
                   k:=0;{numerele sunt prime}
                   for j:=2 to i-1 do begin
                                      if i mod j=0 then k:=1
                                      end;
                   if k=0 then begin
                               writeln(i);
                               n:=n+1;
                               end;
                 end;
writeln('Numarul numerelor prime de la 1 la numarul introdus este  ',n);
while not keypressed do;
readln;
end.
