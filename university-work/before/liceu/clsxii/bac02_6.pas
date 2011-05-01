Program Subiectul_VI;
uses crt;

var s, sol_finala: array[1..11] of integer;
    a: array [1..10, 1..10] of integer;
    i, j, n, cost: integer;

Procedure Calc_cost;
var c, j: integer;

begin
   c := 0;
   for j := 1 to n do
      c := c+a[s[j], s[j+1]];
   if c < cost then
   begin
      cost := c;
      sol_finala := s;
   end;
end;

Procedure Scrie;
var j: integer;
begin
   write(' Drumul de cost minim: ');
   for j := 1 to n+1 do
      write(sol_finala[j], ' ');
   writeln(' cu costul ', cost);
end;

Function Ok(i: integer): boolean;
var j: integer;
begin
   Ok := true;
   for j := 2 to i-1 do
      if s[i] = s[j] then
         Ok := false;
   if (s[i] = 1) and (i < n+1) then
      Ok := false;
end;

Procedure Parcurgere;
begin
   i := 2;
   s[i] := 0;
   s[1] := 1;
   repeat
      while s[i] < n do
      begin
         s[i] := s[i]+1;
         if Ok(i) then
            if i = n+1 then
               Calc_cost
            else
            begin
               i := i+1;
               s[i] := 0;
            end;
      end;
      i := i-1;
   until i = 1;
end;

begin
   clrscr;
   writeln;
   write(' nr. de orase = ');
   readln(n);
   for i := 1 to n do
      for j := i to n do
         if i <> j then
         begin
            write(' costul de depl. din or. ', i, ' in or. ', j, ': ');
            readln(a[i,j]);
            a[j,i] := a[i,j];
         end
         else
         begin
            a[i,j] := maxint;
            a[j,i] := maxint;
         end;
   cost := maxint;
   parcurgere;
   scrie;

   readln;
end.