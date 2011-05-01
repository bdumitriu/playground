Program _3;
uses crt;

var n: longint;
    i, a, c: integer;

Function put(j: integer): longint;
var k: integer;
    p: longint;
begin
   p := 1;
   for k := 1 to j do
      p := p*10;
   put := p;
end;

begin
   clrscr;
   writeln;
   write(' Numarul de cifre: ');
   readln(c);
   n := 0;
   for i := 1 to c do
   begin
      write(' Cifra ', i, ': ');
      readln(a);
      n := n+put(c-i)*a;
   end;
   write(' Numarul este ', n, '.');

   readkey;
end.