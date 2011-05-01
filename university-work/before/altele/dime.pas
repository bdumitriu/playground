Program Interesant;
uses crt;

var i, j, count: integer;
    pass: string;
    c: char;

begin
   clrscr;
   writeln;
   write(' Password: ');
   pass := '';
   c := readkey;
   while c <> #13 do
   begin
      write('*');
      pass := pass+c;
      c := readkey;
   end;
   writeln;
   if pass = #105#110#97#103#118#118#64#98#33 then
   begin
      repeat
         write(' Question: ');
         readln(pass);
         count := 0;
         for i := 1 to 30000 do
         begin
            j := random(10000);
            if j mod 2 = 0 then
               inc(count);
         end;
         if count > 15000 then
            writeln(' YES');
         if count = 15000 then
            writeln(' CAN''T TELL...');
         if count < 15000 then
            writeln(' NO - ', count);
         repeat
            write(' Another ? (y/n) ');
            readln(c);
         until (c = 'y') or (c = 'Y') or (c = 'n') or (c = 'N');
      until (c = 'n') or (c = 'N');
   end
   else
      write(' Sorry, wrong password, try again');

   readkey;
end.