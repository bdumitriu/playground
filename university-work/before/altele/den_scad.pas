Program de_matematica_pentru_Deinsa;
uses crt;
var a, b, i, k, j: integer;
    n: real;

begin
 randomize;
 k:= 0;
 j:= 0;
 repeat
  textbackground(black);
  textcolor(lightgray);
  clrscr;
  a:= random(9);
  b:= random(9);
  gotoxy(33,12);
  textcolor(red);
  if a>=b then
   begin
    write(a,' - ',b,' = ');
    textbackground(lightblue);
    write('  ');
    gotoxy(wherex-2,wherey);
    readln(i);
    if i=a-b then
              begin
               textbackground(black);
               textcolor(green);
               clrscr;
               gotoxy(33,12);
               write('CORECT');
               sound(500);
               delay(1500);
               nosound;
               textcolor(lightgray);
               gotoxy(20,17);
               write('Apasati orice tasta pentru continuare.');
               readkey;
               inc(j);
              end
             else
              begin
               textbackground(black);
               textcolor(green);
               clrscr;
               gotoxy(33,12);
               write('GRESIT');
               textcolor(lightgray);
               gotoxy(33,13);
               write(a,' - ',b,' = ',a-b);
               sound(1500);
               delay(1500);
               nosound;
               gotoxy(20,17);
               write('Apasati orice tasta pentru continuare.');
               readkey;
              end;
    inc(k);
   end;
 until k=20;
 textbackground(black);
 textcolor(magenta);
 clrscr;
 gotoxy(33,12);
 n:= j/2;
 if j mod 2 = 0 then write('Nota este ',trunc(n),'.')
                else write('Nota este ',trunc(n),',50.');
 readkey;
 readkey;
 textcolor(lightgray);
 textbackground(black);
 clrscr;
end.