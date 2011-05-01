Program _2;
uses crt;

var i, n, n1, m: longint;
    a: array[1..10] of integer;

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
   write(' Numarul : ');
   readln(n);
   i := 0;
   n1 := n;
   repeat
      inc(i);
      a[i] := n mod 10;
      n := n div 10;
   until n = 0;
   m := i;
   for i := 1 to m do
      n := n+put(m-i)*a[i];
   if n = n1 then
      write(' Numarul e palindrom.')
   else
      write(' Numarul nu e palindrom.');

   readkey;
end.