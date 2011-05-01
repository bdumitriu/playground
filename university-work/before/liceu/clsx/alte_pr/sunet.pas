program sunet;
uses dos, crt, drivers;
var gata : byte;
var inalt, pasul: word; tasta : char;
begin
 inalt := 100;
 pasul := 25;
 gata := 0;
 clrscr;
 write('De la ce frecventa incep (Hz) ? ');
 readln(inalt);

 if inalt = 0
  then gata:=1;
 write('Pasul ? ');
 readln(pasul);
 if pasul = 0
  then gata := 1;
 if gata = 0
  then begin
            clrscr;
            gotoxy(1,1);
            writeln('Inceput de la: ',inalt);
            gotoxy(1,2);
            writeln('Pasul: ',pasul);
            gotoxy(1,3);
            writeln('Tastati + pentru cresterea freventei, - scadarea ei, ESC iesire');
       end;
 while gata = 0 do
 begin
  sound(inalt);
  if keypressed
   then
    begin
     tasta := readkey;
     case tasta of
      #27 :
            gata := 1;
      '+':
           inalt := inalt + pasul;
      '-':
           inalt := inalt - pasul;
     end;
     gotoxy(1,5);
     clreol;
     write('Frecventa curenta: ',inalt);
    end;
 end;
 nosound;
end.