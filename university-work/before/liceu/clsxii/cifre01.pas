Program _1;
uses crt;

var n, i: longint;
    a: array[1..5] of integer;

begin
   clrscr;
   writeln;
   write(' Numarul : ');
   readln(n);
   i := 0;
   repeat
      inc(i);
      a[i] := n mod 10;
      n := n div 10;
   until n = 0;
   n := a[5]*1000+a[4]*100+a[2]*10+a[1];
   write(' Numarul dupa eliminarea cifrei din mijloc: ', n);

   readkey;
end.