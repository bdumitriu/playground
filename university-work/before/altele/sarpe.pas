Program sarpe;
uses crt, mouse, windos;
type punct=object
            x,y:shortint;
            constructor init;
            Procedure sus;
            Procedure jos;
            Procedure dr;
            Procedure st;
           end;
     ecran=array[1..76,1..23] of 0..1;
var p:punct;
    i,j:integer;
    test:boolean;
    ecr:ecran;
    c:char;

Procedure cursor_off;
var regs:tregisters;
begin
  regs.ax:=$0100;
  regs.cx:=$2607;
  intr(16,regs);
end;

Procedure cursor_on;
var regs:tregisters;
begin
  regs.ax:=$0100;
  regs.cx:=$0506;
  intr(16,regs);
end;


Constructor punct.init;
begin
 x:=38;
 y:=11;
 gotoxy(x,y);
 write('þ');
end;

Procedure punct.sus;
begin
 gotoxy(wherex-1,wherey);
 write(' ');
 if y>-1 then dec(y);
 if y>0 then
  begin
   gotoxy(x,y);
   write('þ');
  end;
end;

Procedure punct.jos;
begin
 gotoxy(wherex-1,wherey);
 write(' ');
 if y<24 then inc(y);
 if y<23 then
  begin
   gotoxy(x,y);
   write('þ');
  end;
end;

Procedure punct.st;
begin
 gotoxy(wherex-1,wherey);
 write(' ');
 if x>-1 then dec(x);
 if x>0 then
  begin
   gotoxy(x,y);
   write('þ');
  end;
end;

Procedure punct.dr;
begin
 gotoxy(wherex-1,wherey);
 write(' ');
 if x<77 then inc(x);
 if x<76 then
  begin
   gotoxy(x,y);
   write('þ');
  end;
end;

begin
 clrscr;
 minit;
 cursor_off;
 textcolor(cyan);
 for i:=1 to 76 do
  for j:=1 to 23 do
   ecr[i,j]:=0;
 write(' Ú');
 gotoxy(79,1);
 write('¿');
 for i:=3 to 78 do
  begin
   gotoxy(i,1);
   write('Ä');
   gotoxy(i,25);
   write('Ä');
  end;
 for i:=2 to 24 do
  begin
   gotoxy(2,i);
   write('³');
   gotoxy(79,i);
   write('³');
  end;
 gotoxy(2,25);
 write('À');
 gotoxy(79,25);
 write('Ù');
 window(3,2,78,24);
 test:=true;
 textcolor(lightgray);
 p.init;
 repeat
  c:=readkey;
  if c=#0 then
           begin
            c:=readkey;
            case c of
             #75 :
              repeat
               p.st;
               delay(300);
               if p.x=0 then test:=false;
              until (keypressed or not(test));
             #77 :
              repeat
               p.dr;
               delay(300);
               if p.x=77 then test:=false;
               until (keypressed or not(test));
             #80 :
              repeat
               p.jos;
               delay(300);
               if p.y=24 then test:=false;
              until (keypressed or not(test));
             #72 :
              repeat
               p.sus;
               delay(300);
               if p.y=0 then test:=false;
              until (keypressed or not(test));
            end;
           end;
 until ((c=#27) or not(test));
 if test=false then
                begin
                 gotoxy(p.x,p.y);
                 textcolor(magenta);
                 write(#15);
                 sound(350);
                 delay(2500);
                 nosound;
                 clrscr;
                 textcolor(red);
                 gotoxy(28,9);
                 write('YOU HAVE FAILED !!!');
                 delay(3300);
                 textcolor(128+blue);
                 gotoxy(25,10);
                 write('PRESS ANY KEY TO EXIT !!!');
                 readkey;
                end;
end.
