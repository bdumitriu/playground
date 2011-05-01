Program factorial_recursiv;
uses crt;
type nat=1..maxlongint;
var n:byte;

Function fact(n:byte):nat;
begin
if n=0 then fact:=1
       else fact:=n*fact(n-1);
end;

begin
clrscr;
write('Introduceti numarul al carui factorial doriti sa-l aflati : ');readln(n);
write(n,'!=',fact(n));
readkey;
end.
