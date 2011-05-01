Program linie;
uses crt;
const spatiu=' ';
var a:char;
    b:integer;
begin
b:=0;
writeln('Acesta este un program care daca ii veti introduce un rand de cuvinte');
writeln('despartite intre ele cu unul sau mai multe spatii va va afisa numarul');
writeln('total de cuvinte din sirul introdus de dumneavoastra');
writeln('Introduceti va rog sirul');
while not eoln do begin
                  read(a);
                  while a=spatiu do
                  read(a);
                  if a<>spatiu then
                  begin b:=b+1;
                  while not eoln and (a<>spatiu) do
                  read(a)
                  end;
                  end;
writeln('Numarul de cuvinte este ',b);
while not keypressed do;
readln;
end.