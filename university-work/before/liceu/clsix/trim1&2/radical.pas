Program Radical;
USES CRT;
var x:real;
begin
Writeln('Acesta este un program de scoatere a radicalului dintr-un numar');
Writeln('Introduceti un numar x');
Write('x=');read(x);
if x>=0 then
begin
  Writeln('Radical din x=',sqrt(x));
end
else
begin
  Writeln('Radical din x este un numar complex.');
  Writeln('Radical din valoarea absoluta a lui x=',sqrt(-x));
end;
while not keypressed do;
readln;
end.