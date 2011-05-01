Program linie2;
uses crt;
var car1,car2:char;
    numar:0..maxint;
begin
writeln('Acesta este un program care daca ii veti introduce o linie de cu-');
writeln('vinte despartite intre ele prin unul sau mai multe spatii si nici');
writeln('un alt semn de punctuatie va va spune cate cuvinte ati introdus');
numar:=0 ; write('Introduceti linia : ');
car1:=' ' ;car2:=' ';
while not eoln do
begin
car1:=car2;
read(car2);
if (car1=' ') and (car2<>' ') then numar:=numar+1;
end;
writeln('Linia cuprinde ',numar,' cuvinte');
while not keypressed do;
end.