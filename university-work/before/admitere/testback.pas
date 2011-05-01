Program test_back;

{Enuntul problemei rezolvate se afla in culegerea de Doina Rancea, vol. 2 la
pagina 14 - programul VI.9}

uses crt;

var n, i, k: integer;
    l, s, cub: array[1..100] of integer;
    c: array[1..100] of string[10];

Procedure Scrie;
var j: integer;
begin
   for j := 1 to k do
      write(s[j], ' ');
   writeln;
end;

Function Ok(i: integer): boolean;
var j: integer;
begin
   Ok := true;
   for j := 1 to i-1 do
      if s[j] = s[i] then
         Ok := false;
   if i > 1 then
   begin
      if l[s[i]] > l[s[i-1]] then
         Ok := false;
      if c[s[i]] = c[s[i-1]] then
         Ok := false;
   end;
end;

Procedure back_ner;
begin
   i := 1;
   s[i] := 0;
   repeat
      while s[i] < n do
      begin
         inc(s[i]);
         if Ok(i) then
         if i = k then
            Scrie
         else
         begin
            inc(i);
            s[i] := 0;
         end;
      end;
      dec(i);
   until i = 0;
end;

Procedure back_rec(i: integer);
var j: integer;
begin
   for j := 1 to n do
   begin
      s[i] := j;
      if Ok(i) then
         if i = k then
            Scrie
         else
            back_rec(i+1);
   end;
end;

Procedure aranjare;
var tst: boolean;
    aux1: integer;
    aux2: string[10];
begin
   repeat
      tst := false;
      for i := 1 to n-1 do
      begin
         if l[i] < l[i+1] then
         begin
            tst := true;
            aux1 := l[i];
            l[i] := l[i+1];
            l[i+1] := aux1;
            aux2 := c[i];
            c[i] := c[i+1];
            c[i+1] := aux2;
            aux1 := cub[i];
            cub[i] := cub[i+1];
            cub[i+1] := aux1;
         end;
      end
   until tst = false;
end;

begin
   clrscr;
   writeln;
   write('n, k sunt: ');
   readln(n, k);
   write('Cuburile: ');
   for i := 1 to n do
   begin
      write(i, ' ');
      cub[i] := i;
   end;
   writeln;
   write('Laturile: ');
   for i := 1 to n do
      read(l[i]);
   readln;
   writeln('Culorile:');
   for i := 1 to n do
   begin
      write('    cubul ', i, ' ');
      readln(c[i]);
   end;
   writeln;
   back_rec(1);
{   aranjare;}
   readkey;
end.