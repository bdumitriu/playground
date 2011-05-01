uses crt;
type sex = (f, m);
     art = record
              n: string[15];
              s: sex;
           end;

var fis: FILE of art;
    nr, i: integer;
    p, q: array[1..10] of art;
    c: char;

begin
   clrscr;
   assign(fis, 'pers.dat');
   reset(fis);
   write(' nr = ');
   readln(nr);
   for i := 1 to nr do
      with p[i] do
      begin
         write(' Numele persoanei ', i, ': ');
         readln(n);
         write(' Sexul persoanei ', i, ': ');
         readln(c);
         if (c = 'm') or (c = 'M') then
            s := m;
         if (c = 'f') or (c = 'F') then
            s := f;
      end;
   for i := 1 to nr do
      with p[i] do
      begin
         write(i, ':  nume = ', n);
         if s = m then
            writeln('; sex = masculin.')
         else
            writeln('; sex = feminin.');
         write(fis, p[i]);
      end;

   for i := 1 to filesize(fis) do
      with q[i] do
      begin
         read(fis, q[i]);
         write(i, ':  nume = ', n);
         if s = m then
            writeln('; sex = masculin.')
         else
            writeln('; sex = feminin.');
      end;
   close(fis);

   readkey;
end.