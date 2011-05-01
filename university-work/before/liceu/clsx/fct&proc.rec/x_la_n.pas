Program numar_la_o_putere;
uses crt;
var x:real;
    n:byte;

function x_la_puterea_n(x:real;n:byte):real;
begin
if n=0 then x_la_puterea_n:=1
       else x_la_puterea_n:=x_la_puterea_n(x,n-1)*x;
end;

begin
clrscr;
write('Introduceti baza : ');readln(x);
write('Introduceti exponentul : ');readln(n);
write(x,' la puterea ',n,' este : ',x_la_puterea_n(x,n));
readkey;
end.