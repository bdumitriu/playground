Program nume_etc;
uses crt;
label 1,2,3,4,5,6,7,8,9,10,11,12;
type pers=record
           nume,prenume,str,bl,tel,loc,tara:string[15];
           nr,ap,cod:string[5];
           sc,et:string[3];
          end;
var n:char;
    f:file of pers;
    x:integer;
    om:pers;
    test:boolean;

begin

 clrscr;

 textbackground(green);
 clrscr;
 writeln;
 textcolor(red);
 write(' Numele : ');
 textbackground(blue);
 write('               ');
 textbackground(green);
 write(' Prenumele : ');
 textbackground(blue);
 write('               ');
 gotoxy(1,4);
 textbackground(green);
 write(' Strada : ');
 textbackground(blue);
 write('               ');
 textbackground(green);
 write(' Numarul : ');
 textbackground(blue);
 write('     ');
 gotoxy(1,6);
 textbackground(green);
 write(' Blocul : ');
 textbackground(blue);
 write('               ');
 textbackground(green);
 write(' Scara : ');
 textbackground(blue);
 write('   ');
 textbackground(green);
 write(' Etajul : ');
 textbackground(blue);
 write('   ');
 textbackground(green);
 write(' Apartamentul : ');
 textbackground(blue);
 write('     ');
 gotoxy(1,8);
 textbackground(green);
 write(' Codul postal : ');
 textbackground(blue);
 write('      ');
 textbackground(green);
 write(' Localitatea : ');
 textbackground(blue);
 write('               ');
 gotoxy(1,10);
 textbackground(green);
 write(' Telefonul : ');
 textbackground(blue);
 write('                ');
 textbackground(green);
 write(' Tara : ');
 textbackground(blue);
 write('               ');
 assign(f,'agenda.dat');
 reset(f);
 om.nume:='';
 om.prenume:='';
 om.str:='';
 om.bl:='';
 om.tel:='';
 om.loc:='';
 om.tara:='';
 om.nr:='';
 om.ap:='';
 om.ap:='';
 om.cod:='';
 om.sc:='';
 om.et:='';
 window(1,24,80,25);
 textbackground(black);
 clrscr;
 textcolor(brown);
 gotoxy(12,1);
 write('Apasati "ESC" pt. scrierea datelor in fisier si iesire, ');
 gotoxy(8,2);
 write('"TAB" pt. casuta urmatoare, "SHIFT"+"TAB" pt. casuta precedenta.');
 textbackground(blue);
 textcolor(yellow);
 1:
 window(11,2,25,2);
 test:=false;
 clrscr;
 write(om.nume);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                                        begin
                                                         om.nume:=om.nume+n;
                                                         write(n);
                                                        end;
  if n=#8 then
           begin
            x:=length(om.nume);
            om.nume:=copy(om.nume,1,x-1);
            clrscr;
            write(om.nume);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);
             write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 12;
 2:
 window(39,2,53,2);
 test:=false;
 clrscr;
 write(om.prenume);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.prenume:=om.prenume+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.prenume);
            om.prenume:=copy(om.prenume,1,x-1);
            clrscr;
            write(om.prenume);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 1;
 3:
 window(11,4,25,4);
 test:=false;
 clrscr;
 write(om.str);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.str:=om.str+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.str);
            om.str:=copy(om.str,1,x-1);
            clrscr;
            write(om.str);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 2;
 4:
 window(37,4,41,4);
 test:=false;
 clrscr;
 write(om.nr);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.nr:=om.nr+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.nr);
            om.nr:=copy(om.nr,1,x-1);
            clrscr;
            write(om.nr);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 3;
 5:
 window(11,6,25,6);
 test:=false;
 clrscr;
 write(om.bl);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.bl:=om.bl+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.bl);
            om.bl:=copy(om.bl,1,x-1);
            clrscr;
            write(om.bl);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 4;
 6:
 window(35,6,37,6);
 test:=false;
 clrscr;
 write(om.sc);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.sc:=om.sc+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.sc);
            om.sc:=copy(om.sc,1,x-1);
            clrscr;
            write(om.sc);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 5;
 7:
 window(48,6,50,6);
 test:=false;
 clrscr;
 write(om.et);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.et:=om.et+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.et);
            om.et:=copy(om.et,1,x-1);
            clrscr;
            write(om.et);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 6;
 8:
 window(67,6,71,6);
 test:=false;
 clrscr;
 write(om.ap);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.ap:=om.ap+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.ap);
            om.ap:=copy(om.ap,1,x-1);
            clrscr;
            write(om.ap);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 7;
 9:
 window(17,8,22,8);
 test:=false;
 clrscr;
 write(om.cod);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.cod:=om.cod+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.cod);
            om.cod:=copy(om.cod,1,x-1);
            clrscr;
            write(om.cod);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 8;
 10:
 window(38,8,52,8);
 test:=false;
 clrscr;
 write(om.loc);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.loc:=om.loc+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.loc);
            om.loc:=copy(om.loc,1,x-1);
            clrscr;
            write(om.loc);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 9;
 11:
 window(14,10,28,10);
 test:=false;
 clrscr;
 write(om.tel);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.tel:=om.tel+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.tel);
            om.tel:=copy(om.tel,1,x-1);
            clrscr;
            write(om.tel);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 10;
 12:
 window(38,10,52,10);
 test:=false;
 clrscr;
 write(om.tara);
 repeat
  n:=readkey;
  if n=#0 then
           begin
            n:=readkey;
            if n=#15 then test:=true;
           end;
  if (n<>#8) and (n<>#9) and (n<>#27) and (test=false) then
                                       begin
                                        om.tara:=om.tara+n;
                                        write(n);
                                       end;
  if n=#8 then
           begin
            x:=length(om.tara);
            om.tara:=copy(om.tara,1,x-1);
            clrscr;
            write(om.tara);
           end;
  if n=#27 then
            begin
             window(1,1,80,25);write(f,om);
             close(f);
             textbackground(black);
             clrscr;
             textcolor(lightgray);
             halt;
            end;
 until (n=#9) or (test);
 if test then goto 11;
 goto 1;
end.