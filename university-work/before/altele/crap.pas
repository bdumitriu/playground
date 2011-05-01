Program Interesant_facut_cu_o_seara_inainte_de_XMas_23_12_1998_that_is;
uses crt;
var test: boolean;
    c: char;
    i: integer;

begin

   clrscr;
   writeln;

   writeln(' This is a program which does nothing so if you are a real busy');
   writeln('person I strongly suggest that you exit this crappy creation of');
   writeln('mine right this instant. I really wouldn''t like to get phone');
   writeln('calls from all sort of people who want to tell me that they have');
   writeln('lost an hour of their pretious time trying to find some sort of');
   writeln('use for this program only to eventually realise that it is completely');
   writeln('useless.');
   writeln(' So please, if you ain''t in the mood for shitting around press');
   writeln('          the EXIT button and get it over with.');

   textcolor(red);
   textbackground(yellow);
   gotoxy(20, 22);
   write('<Please, go on>');
   textbackground(black);
   gotoxy(45, 22);
   write('< EXIT >');

   test := false;
   i := 1;
   repeat
      c := readkey;
      if c = #0 then
         c := readkey;
         if (c = #75) or (c = #77) or (c = #72) or (c = #80) then
         begin
            if i = 1 then
            begin
               gotoxy(20, 22);
               textbackground(black);
               write('<Please, go on>');
               i := 2;
               gotoxy(45, 22);
               textbackground(yellow);
               write('< EXIT >');
            end
            else
            begin
               gotoxy(20, 22);
               textbackground(yellow);
               write('<Please, go on>');
               i := 1;
               gotoxy(45, 22);
               textbackground(black);
               write('< EXIT >');
            end;
         end
      else
         if c = #13 then
            test := true;
   until test = true;

end.