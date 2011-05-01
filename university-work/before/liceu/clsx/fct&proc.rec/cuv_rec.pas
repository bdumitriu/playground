Program inversare_de_mai_multe_cuvinte;
uses crt;
var n:byte;

Procedure invers(i:byte);
var a:string;
begin
 readln(a);
 if i<n then invers(i+1)
        else writeln('Cuvintele in ordine inversa');
 writeln(a);
end;

begin
 clrscr;
 writeln;
 write('numarul de cuvinte este ');readln(n);
 invers(1);
 readkey;
end.
