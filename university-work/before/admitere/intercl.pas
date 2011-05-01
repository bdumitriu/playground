uses crt;

var s: array[1..100] of -127..128;
    n, i, j: integer;

Procedure Intercl(li, ls: integer);
begin
   {to be implemented}
end;

Procedure DivImp(li, ls);
begin
   if ls > li then
   begin
      DivImp(li, (li+ls) div 2);
      DivImp(((li+ls) div 2)+1, ls);
      Intercl(li, ls);
   end;
end;

begin
   clrscr;
   writeln;

   write(' n = ');
   readln(n);
   for i := 1 to n do
   begin
      write('s[', i, '] = ');
      readln(s[i]);
   end;
   DivImp(1, n);

   readkey;
end.