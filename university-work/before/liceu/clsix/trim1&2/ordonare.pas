program ordonare;
uses crt;
var a,b,c:longint;
begin
writeln('Acesta este un program care daca ii veti introduce 3 numere vi le');
writeln('va afisa pe acestea in ordine crescatoare');
writeln('');
writeln('Introduceti va rog cele trei numere');
write('a=');read(a);
write('b=');read(b);
write('c=');read(c);
if a>b then begin
            if b>c then write(c,',',b,',',a)
                   else begin
                        if a>c then writeln(b,',',c,',',a)
                               else writeln(b,',',a,',',c)
                        end;
            end
       else begin
            if a>c then writeln(c,',',a,',',b)
                   else begin
                        if b>c then write(a,',',c,',',b)
                               else write(a,',',b,',',c);
                        end;
            end;
while not keypressed do;
readln;
end.