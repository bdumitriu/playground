Program Metoda_tangentei;
uses crt;

var a, b, x, y, aux: real;

Function f(x: real): real;
begin
   f := sqr(x)-5;
end;

Function fder(x: real): real;
begin
   fder := 2*x;
end;

begin
   clrscr;
   writeln;
   write(' marginea inferioara = ');
   readln(a);
   write(' marginea superioara = ');
   readln(b);
   aux := b;
   repeat
      x := aux;
      y := x-f(x)/fder(x);
      aux := y;
   until (abs(x-y) < 0.001) or (keypressed);
   writeln(' Solutia cu aproximare de trei zecimale este: ', (x-y)/2);

   readkey;
end.