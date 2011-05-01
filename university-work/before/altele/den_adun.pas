Program de_matematica_pentru_Deinsa;
uses crt;
var a, b, i, k, j, x: integer;
    n: real;
    sir: array[1..4] of char;

begin
 randomize;
 k:= 0;
 j:= 0;
 repeat
  textbackground(black);
  textcolor(lightgray);
  clrscr;
  repeat
     a:= random(100);
     b:= random(9);
  until (a mod 10+b < 10);
  gotoxy(5,2);
  if (10-k <> 1) then
     write('Denisa, mai ai de facut ',10-k,' exercitii !!!')
  else
     write('Denisa, mai ai de facut 1 exercitiu !!!');
  gotoxy(33,12);
  textcolor(red);
  write(a,' + ',b,' = ');
  textbackground(lightblue);
  write('   ');
  gotoxy(wherex-2,wherey);
  i:= 1;
  repeat
     sir[i]:= readkey;
     if ord(sir[i]) in [48..57]
        then
           begin
              write(sir[i]);
              inc(i);
           end;
  until sir[i] = chr(13);
  case i of
     1:
        x:= ord(sir[i])-ord('0');
     2:
        x:= (ord(sir[1])-ord('0'))*10+(ord(sir[2])-ord('0'));
     3:
        x:= (ord(sir[1])-ord('0'))*100+(ord(sir[2])-ord('0'))*10+(ord(sir[3])-ord('0'));
  if x=a+b then
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
             write(a,' + ',b,' = ',a+b);
             sound(1500);
             delay(1500);
             nosound;
             gotoxy(20,17);
             write('Apasati orice tasta pentru continuare.');
             readkey;
            end;
  inc(k);
 until k=2;
 textbackground(black);
 textcolor(magenta);
 clrscr;
 gotoxy(33,12);
 n:= j;
{ n:= j/2;
 if j mod 2 = 0 then }write('Nota este ',trunc(n),'.');
{                else write('Nota este ',trunc(n),',50.');}
 readkey;
 readkey;
 textcolor(lightgray);
 textbackground(black);
 clrscr;
end.