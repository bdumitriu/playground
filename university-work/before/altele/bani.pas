uses crt;
var s, d: real;
    c: char;
    i: integer;
begin
   clrscr;
   writeln;

   write(' Suma: ');
   readln(s);
   write(' Dobanda pe perioada: ');
   readln(d);
   s := s+s*d/100+100000;
   writeln(' Suma dupa perioada 1 (cu dobanda adaugata) este ', s:7:0);
   write(' Mai doriti (d/n)? ');
   readln(c);
   i := 1;
   while (c = 'd') or (c = 'D') do
   begin
      inc(i);
      s := s+s*d/100+100000;
      writeln(' Suma dupa perioada ', i, ' (cu dobanda adaugata) este ', s:7:0);
      write(' Mai doriti (d/n)? ');
      readln(c);
   end;
end.