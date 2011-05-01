Program QuickSort;
uses crt;

var i, n, temp: integer;
    a: array[1..100] of integer;

Procedure quick(inf, sup: integer);
var i, j, piv: integer;
begin
   piv := a[(inf+sup) div 2];
   i := inf;
   j := sup;
   repeat
      while a[i] < piv do
         inc(i);
      while a[j] > piv do
         dec(j);
      if i <= j then
      begin
         temp := a[i];
         a[i] := a[j];
         a[j] := temp;
         inc(i);
         dec(j);
      end;
   until i > j;
   if j > inf then
      quick(inf, j);
   if i < sup then
      quick(i, sup);
end;

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

   quick(1,n);

   writeln(' Sirul sortat este:');
   for i := 1 to n do
      write(a[i], ' ');
   readkey;
end.