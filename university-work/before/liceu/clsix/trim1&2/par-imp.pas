Program pare;
uses crt;
type vect=array[1..10] of integer;
var i,k,p:integer;{k reprezinta numarul de numere pare iar p numarul
                   de numere impare}
    x:vect;
begin
writeln('Acesta este un program care daca ii veti introduce 10 numere');
writeln('va va spune cate dintre cele diferte de 0 sunt pare si cate');
writeln('sunt impare');
writeln('Introduceti numerele');
k:=0;
p:=0;
for i:=1 to 10 do begin
                  write('x[',i,']=');read(x[i]);
                  end;
for i:=1 to 10 do begin
if x[i] <>0 then begin
                 if x[i]/2=(x[i] div 2) then k:=k+1
                                        else p:=p+1;
                 end
            else k:=k;
                 p:=p;
                 end;
Writeln('numarul de numere pare este ',k);
Writeln('numarul de numere impare este ',p);
while not keypressed do;
readln;
end.