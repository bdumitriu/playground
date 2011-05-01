Program ShellSort;
uses crt;

var i, j, n, incr, temp: integer;
    v: boolean;
    a: array[1..100] of integer;

begin
   clrscr;
   writeln;
   write(' Numarul de elemente din sir: ');
   readln(n);
   for i := 1 to n do
   begin
      write('  elementul ', i, ': ');
      readln(a[i]);
   end;
   incr := 1;
   while incr <= n do
      incr := incr*3+1;
   repeat
      incr := incr div 3;
      for i := incr+1 to n do
      begin
         j := i;
         temp := a[i];
         v := false;
         while (not v) and (a[j-incr] > temp) do
         begin
            a[j] := a[j-incr];
            j := j-incr;
            if j <= incr then
               v := true;
         end;
         a[j] := temp;
      end;
   until incr = 1;
   writeln(' Sirul sortat este:');
   for i := 1 to n do
      write(a[i], ' ');
   readkey;
end.