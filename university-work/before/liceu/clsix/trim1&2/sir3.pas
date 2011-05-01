Program sir3;
uses crt;
type vect=array[1..10] of integer;
var a:vect;
    i:integer;
begin
writeln('Acesta este un program care executa sirul lui Fibonacci(prea greu');
writeln('ca sa explic ce-i ala)');
a[1]:=1;
a[2]:=2;
for i:=3 to 10 do begin
                 a[i]:=a[i-2]+a[i-1];
                 end;
for i:=1 to 10 do writeln('a[',i,']=',a[i]);
while not keypressed do;
end.
