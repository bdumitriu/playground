Program Subiectul_V;
uses crt;

type elev = record
        medie: real;
        nume, prenume: string[30];
     end;

var f: text;
    i, n: integer;
    a: array [1..100] of elev;
    str: string;

Procedure Ordonare;
var aux: elev;
    test: boolean;
begin
   repeat
      test := true;
      for i := 1 to n-1 do
         if a[i].medie < a[i+1].medie then
         begin
            aux := a[i];
            a[i] := a[i+1];
            a[i+1] := aux;
            test := false;
         end;
   until test = true;
end;

begin
   clrscr;
   writeln;
   assign(f, 'medii.txt');
   reset(f);
   i := 0;
   while not eof(f) do
   begin
      i := i+1;
      readln(f, str);
      tst := true;
      for i := 1 to length(str) do

         if s[i] = ' ' then
         begin
            tst := false;
            num := i-1;

   end;
   close(f);
   n := i;
   Ordonare;
   for i := 1 to n do
      if (a[i].medie >= 4.50) then
         writeln(' ', a[i].nume, ' ', a[i].prenume, ' ', a[i].medie)
      else
         i := n+1;


   readln;
end.