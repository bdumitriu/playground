Program de_platire_a_unei_sume_in_monede_diferite;
uses crt;
var n, i, x, q: integer;
    a, b, s: array[1..100] of integer;

Procedure scrie(i: byte);
var z: integer;
begin
   q := q+1;
   writeln(' Solutia ', q, ' este :');
   for z := 1 to i do
      if s[z] <> 0 then
      begin
         write(s[z]);
         if s[z] = 1 then
            write(' moneda de ')
         else
            write(' monede de ');
         write(a[z]);
         if a[z] = 1 then
            write(' leu.')
         else
            write(' lei.');
         writeln;
      end;
   readln;
end;

Function spa(i: byte): integer; {spa = suma pana acum}
var y: byte;
    c: integer;
begin
   c := 0;
   for y := 1 to i do
      c := c+(a[y]*s[y]);
   spa := c;
end;

Procedure suma(i: byte);
var j: integer;
begin
   for j := 0 to b[i] do
   begin
      s[i] := j;
      if spa(i) = n then
         scrie(i)
      else
         if (spa(i) < n) and (i < x) then
            suma(i+1);
  end;
end;

begin
   clrscr;
   writeln;

   q := 0;
   write(' Suma care trebuie platita este ');
   readln(n);
   write(' Cate tipuri de monede vom avea ? ');
   readln(x);
   for i := 1 to x do
   begin
      write(' Moneda ', i, ' : ');
      readln(a[i]);
   end;
   for i := 1 to x do
      b[i] := n div a[i];
   suma(1);
end.