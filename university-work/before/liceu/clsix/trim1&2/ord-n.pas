Program n;
uses crt;
var x,y:real;
begin
writeln('Acesta este un program de ordonare a valorilor x,x la a 2-a,');
writeln('x la a 3-a,radical din x');
writeln('Introduceti numarul x');
write('x=');read(x);
if x>=0 then
        writeln('',x,',',x*x,',',x*x*x,',',sqrt(x))
        else
        writeln('Nu putem ordona deoarece radical din x este din C');
while not keypressed do;
readln;
end.

