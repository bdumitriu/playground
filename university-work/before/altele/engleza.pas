Program de_invatare_a_limbii_engleze;
uses crt, utilg, graph, mouse;
label 1,2,3,4,5;
type sir=array[1..25] of string;
var cuv:sir;
    ch:char;
    id:byte;

Procedure antet;
var i:0..15;
begin
 for i:=0 to 15 do setpalette(i,i);
 for i:=0 to 15 do setrgbpalette(i,0,4*i,0);
 for i:=15 downto 0 do
  begin
   setfillstyle(solidfill,15-i);
   bar(0,(getmaxy div 14)*i,getmaxx,(getmaxy div 14)*i+(getmaxy div 14));
  end;
 setcolor(0);
 settextstyle(1,0,7);
 for i:=15 downto 0 do
  begin
   setcolor(i);
   outtextxy(50,100,'ENGLEZA DE BAZA');
   delay(200);
  end;
 settextstyle(2,0,7);
 outtextxy(95,210,'-Cel mai usor mod de a invata engleza-');
 setcolor(15);
 settextstyle(1,0,2);
 outtextxy(210,455,'Copyright (C) -Bogdan Dumitriu- 1997');
end;

Procedure sunet_initial;
type sir=array[1..9] of word;
const frecv:sir=(330,524,494,440,416,440,494,416,330);
var i:integer;
begin
 for i:=1 to 9 do
  begin
   sound(frecv[i]);
   case i of
    1:delay(1300);
    2:delay(1100);
    3:delay(1000);
    4:delay(950);
    5:delay(1200);
    6:delay(1000);
    7:delay(900);
    8:delay(900);
    9:delay(1000);
   end;
   nosound;
  end;
 setcolor(0);
 outtextxy(140,315,'Apasati "ENTER" pentru continuare.');
 readln;
end;

Procedure buton(x1,y1,x2,y2:integer;str:string;tip:byte);
var i:integer;
    hor,ver:real;
begin
 setfillstyle(1,7);
 bar(x1,y1,x2,y2);
 setcolor(8);
 for i:=0 to tip do line(x1+i,y1+i,x2-i,y1+i);
 for i:=0 to tip do line(x2-i,y1+i,x2-i,y2-i);
 setcolor(15);
 for i:=0 to tip do line(x1+i,y2-i,x2-i,y2-i);
 for i:=0 to tip do line(x1+i,y1+i,x1+i,y2-i);
 setcolor(8);
 settextstyle(2,0,6);
 hor:=((x2-x1)/2)+x1-(length(str)*10)/2;
 ver:=((y2-y1)/2)+y1-4;
 outtextxy(round(hor),round(ver)-5,str);
 outtextxy(round(hor)+1,round(ver)-5,str);
 settextstyle(0,0,0);
end;

Procedure copyright_notice;
var i,j:integer;
    test:boolean;
begin
 cleardevice;
 rev;
 setcolor(yellow);
 line(100,getmaxy div 2,getmaxx-100,getmaxy div 2);
 j:=getmaxy div 2;
 for i:=getmaxy div 2 + 1 downto 100 do
  begin
   line(100,i,getmaxx-100,i);
   j:=j+1;
   line(100,j,getmaxx-100,j);
   delay(10);
  end;
 for i:=99 downto 0 do
  begin
   rectangle(i,i,getmaxx-i,getmaxy-i);
   delay(10);
  end;
 setcolor(black);
 settextstyle(6,0,4);
 outtextxy(165,3,'Copyright (C) notice');
 line(152,55,445,55);
 settextstyle(6,0,3);
 outtextxy(7,100,'Acest produs este licentiat lui Bogdan Dumitriu, el detinand');
 delay(1500);
 outtextxy(7,130,'toate drepturile asupra lui. Orice copie, multiplicare sau');
 delay(1500);
 outtextxy(7,160,'alta folosire neautorizata a acestui produs este stict ');
 delay(1500);
 outtextxy(7,190,'interzisa. Pretul produsului este de 25000 lei. Orice actiune');
 delay(1500);
 outtextxy(7,220,'care nu respecta cele mentionate mai sus e impotriva legii');
 delay(1500);
 outtextxy(7,250,'si va fi pedepsita in conformitate cu normele in vigoare.');
 delay(1500);
 outtextxy(7,320,'      Sunteti de acord sa respectati cele de mai sus ? ');
 buton(150,400,250,430,'DA',0);
 buton(300,400,400,430,'NU',0);
 MOn;
 test:=false;
 repeat
 if MStanga then
             begin
              for i:=150 to 250 do
               begin
                if Mgx=i then
                 for j:=400 to 430 do
                  if Mgy=j then
                            begin
                             buton(150,400,250,430,'DA',1);
                             delay(350);
                             test:=true;
                            end;
               end;
              for i:=300 to 400 do
               begin
                if Mgx=i then
                 for j:=400 to 430 do
                  if Mgy=j then
                            begin
                             buton(300,400,400,430,'NU',1);
                             test:=true;
                             delay(350);
                             closegraph;
                             gotoxy(20,10);
                             textbackground(lightblue);
                             textcolor(red);
                             writeln('Atunci nu puteti executa programul!');
                             gotoxy(30,15);
                             textbackground(black);
                             textcolor(lightgray);
                             writeln('Apasati "ENTER" ');
                             readln;
                             halt;
                            end;
               end;
             end;
 until test;
end;

Procedure texte(x,y:integer;str:string;bk,c:word);
begin
 textbackground(bk);
 textcolor(c);
 gotoxy(x,y);
 write(str);
end;

Procedure cursor(x,y,lung:integer;c:word);
var i:integer;
begin
 textcolor(c);
 gotoxy(x,y);
 for i:=1 to lung do
  write(chr(219));
end;

Procedure meniu(cuv:sir;j:byte;var id:byte);
var i,lin:byte;
    ch:char;
begin
 closegraph;
 textcolor(lightgreen);
 gotoxy(26,6);
 write('Ú');write('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');write('¿');
 for i:=7 to j+6 do
  begin
   gotoxy(26,i);write('³');
   gotoxy(50,i);write('³');
  end;
 gotoxy(26,7+j);
 write('À');write('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');write('Ù');
 for i:=1 to j do
  texte(28,6+i,cuv[i],black,magenta);
 lin:=7;
 cursor(27,7,22,lightgray);
 texte(28,7,cuv[1],lightgray,black);
 repeat
  ch:=readkey;
  if ch=#80 then
             begin
              cursor(27,lin,22,black);
              texte(28,lin,cuv[lin-6],black,magenta);
              lin:=lin+1;
              if lin=j+7 then lin:=7;
              cursor(27,lin,22,lightgray);
              texte(28,lin,cuv[lin-6],lightgray,black);
             end;
  if ch=#72 then
             begin
              cursor(27,lin,22,black);
              texte(28,lin,cuv[lin-6],black,magenta);
              lin:=lin-1;
              if lin=6 then lin:=j+6;
              cursor(27,lin,22,lightgray);
              texte(28,lin,cuv[lin-6],lightgray,black);
             end;
 until ch=#13;
 id:=lin;
end;

Procedure curatare;
begin
 textbackground(black);
 clrscr;
end;

Procedure sub_constructie;
begin
 curatare;
 gotoxy(20,10);
 textbackground(lightblue);
 textcolor(red);
 write('Imi pare rau dar aceasta sectiune este');
 gotoxy(20,11);
 write('         inca sub constructie         ');
 delay(500);
 textbackground(black);
 textcolor(lightgray);
 gotoxy(15,15);
 write('Apasati "ENTER" pentru revenire la meniul anterior');
 gotoxy(15,16);
 write('sau orice alta tasta pentru iesire din program.');
end;

Procedure dictionar_scriere(x:string);
label 1,2;
var cuv,str,str2:string;
    i,j,k:integer;
    f,g:text;
begin
 curatare;
 textcolor(lightgray);
 gotoxy(20,11);
 write('Cuvantul : ');
 textcolor(yellow);
 gotoxy(31,10);
 write('Ú');write('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');write('¿');
 gotoxy(31,11);
 write('³                       ³');
 gotoxy(31,12);
 write('À');write('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');write('Ù');
 gotoxy(33,11);
 window(33,11,54,11);
 textcolor(red);
 readln(cuv);
 assign(f,x);
 reset(f);
 i:=-1;
 repeat
  readln(f,str);
  for j:=1 to length(str) do
   if str[j]='=' then
                  begin
                   str2:='';
                   for k:=1 to j-2 do
                    str2:=str2+str[k];
                   j:=length(str);
                  end;
  if str2=cuv then goto 1;
  inc(i);
 until (cuv<str2) or (eof(f));
 assign(g,'auxil.dat');
 rewrite(g);
 close(f);
 reset(f);
 if cuv<str2 then
  for j:=1 to i do
   begin
    readln(f,str);
    writeln(g,str);
   end;
 if eof(f) then
  for j:=1 to i+1 do
   begin
    readln(f,str);
    writeln(g,str);
   end;
 window(1,1,80,25);
 gotoxy(20,14);
 textcolor(lightgray);
 writeln('Definitia : ');
 textcolor(yellow);
 gotoxy(32,13);
 write('Ú');write('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');write('¿');
 gotoxy(32,14);
 write('³                                      ³');
 gotoxy(32,15);
 write('À');write('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');write('Ù');
 gotoxy(34,14);
 textcolor(red);
 window(34,14,70,14);
 readln(str);
 write(g,cuv);
 write(g,' = ');
 write(g,str);
 writeln(g);
 while not eof(f) do
  begin
   readln(f,str);
   writeln(g,str);
  end;
 close(f);
 close(g);
 erase(f);
 rename(g,x);
 window(1,1,80,25);
 goto 2;
 1:
 textbackground(lightblue);
 window(1,1,80,25);
 gotoxy(20,15);
 write('Cuvantul se afla deja in dictionar.');
 close(f);
 delay(500);
 gotoxy(14,20);
 textbackground(black);
 textcolor(lightgray);
 write('Apasati "ENTER" pentru revenire in meniul anterior');
 gotoxy(14,21);
 write('sau orice alta alta tasta pentru incheierea programului.');
 str:=readkey;
 if str<>#13 then halt;
 2:
end;

Procedure dictionar_cautare(x:string);
label 1,2;
var str,str2,cuv:string;
    f:text;
    j,k:integer;
begin
 assign(f,x);
 curatare;
 textcolor(lightgray);
 gotoxy(20,11);
 write('Cuvantul : ');
 textcolor(yellow);
 gotoxy(31,10);
 write('Ú');write('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');write('¿');
 gotoxy(31,11);
 write('³                       ³');
 gotoxy(31,12);
 write('À');write('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');write('Ù');
 gotoxy(33,11);
 window(33,11,54,11);
 textcolor(red);
 readln(cuv);
 reset(f);
 repeat
  readln(f,str);
  str2:='';
  for j:=1 to length(str) do
   if (str[j+1]=' ') and (str[j+2]='=') then
                                         begin
                                          for k:=1 to j do
                                           str2:=str2+str[k];
                                          j:=length(str);
                                         end;
  if str2=cuv then goto 1;
 until (cuv=str2) or (eof(f));
 textbackground(lightblue);
 window(1,1,80,25);
 gotoxy(20,15);
 write('Imi pare rau, cuvantul nu se afla in dictionar.');
 close(f);
 delay(500);
 gotoxy(14,20);
 textbackground(black);
 textcolor(lightgray);
 write('Apasati "ENTER" pentru revenire in meniul anterior');
 gotoxy(14,21);
 write('sau orice alta alta tasta pentru incheierea programului.');
 str:=readkey;
 if str<>#13 then halt;
 goto 2;
 1:
 window(1,1,80,25);
 clrscr;
 gotoxy(2,10);
 textcolor(red);
 textbackground(black);
 write(str);
 delay(500);
 gotoxy(14,20);
 textbackground(black);
 textcolor(lightgray);
 write('Apasati "ENTER" pentru revenire in meniul anterior');
 gotoxy(14,21);
 write('sau orice alta alta tasta pentru incheierea programului.');
 str:=readkey;
 if str<>#13 then halt;
 2:
end;

Procedure dictionar_stergere(x:string);
label 1,2,3;
var f,g:text;
    a,b,i,j:integer;
    str,str2,cuv:string;
begin
 curatare;
 textcolor(lightgray);
 gotoxy(2,11);
 write('Cuvantul pe care doriti sa-l stergeti : ');
 textcolor(yellow);
 gotoxy(42,10);
 write('Ú');write('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');write('¿');
 gotoxy(42,11);
 write('³                       ³');
 gotoxy(42,12);
 write('À');write('ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ');write('Ù');
 gotoxy(44,11);
 window(44,11,65,11);
 textcolor(red);
 readln(cuv);
 assign(f,x);
 reset(f);
 a:=0;
 repeat
  inc(a);
  readln(f,str);
  for j:=1 to length(str) do
   if str[j]='=' then
                  begin
                   str2:='';
                   for i:=1 to j-2 do
                    str2:=str2+str[i];
                   j:=length(str);
                   if str2=cuv then
                    begin
                     b:=0;
                     close(f);
                     assign(g,'fuck_you');
                     rewrite(g);
                     close(g);
                     append(g);
                     reset(f);
                     repeat
                      inc(b);
                      readln(f,str2);
                      if b<>a then writeln(g,str2);
                     until eof(f);
                     close(f);
                     close(g);
                     erase(f);
                     rename(g,x);
                     goto 1;
                    end;
                  end;
 until eof(f);
 window(1,1,80,25);
 close(f);
 textbackground(blue);
 textcolor(red);
 gotoxy(20,15);
 write('Cuvantul nu se afla in dictionar.');
 goto 2;
 1:
 window(1,1,80,25);
 textbackground(blue);
 textcolor(red);
 gotoxy(20,15);
 write('Cuvantul a fost sters din dictionar.');
 2:
 gotoxy(14,20);
 textbackground(black);
 textcolor(lightgray);
 write('Apasati "ENTER" pentru revenire in meniul anterior');
 gotoxy(14,21);
 write('sau orice alta alta tasta pentru incheierea programului.');
 str:=readkey;
 if str<>#13 then halt;
end;

begin
 ini;
 antet;
 sunet_initial;
 copyright_notice;
 1:
 curatare;
 cuv[1]:='MENIU PRINCIPAL';
 cuv[2]:='IESIRE DIN PROGRAM';
 meniu(cuv,2,id);
 if id=8 then halt;
 if id=7 then
  begin
   2:
   curatare;
   cuv[1]:='DICTIONAR';
   cuv[2]:='INVATAREA LITERELOR';
   cuv[3]:='TEST DE LIMBA';
   cuv[4]:='GRAMATICA DE BAZA';
   cuv[5]:='MENIU ANTERIOR';
   meniu(cuv,5,id);
   if id=11 then goto 1;
   if id in [8..10] then
    begin
     sub_constructie;
     ch:=readkey;
     if ch=#13 then goto 2
               else halt;
    end;
   if id=7 then
    begin
     3:
     curatare;
     cuv[1]:='ROMAN-ENGLEZ';
     cuv[2]:='ENGLEZ-ROMAN';
     cuv[3]:='MENIU ANTERIOR';
     meniu(cuv,3,id);
     if id=9 then goto 2;
     if id=7 then
      begin
       4:
       curatare;
       cuv[1]:='CAUTARE DE CUVANT';
       cuv[2]:='INTRODUCERE CUVANT';
       cuv[3]:='STERGERE DE CUVANT';
       cuv[4]:='MENIU ANTERIOR';
       meniu(cuv,4,id);
       if id=10 then goto 3;
       if id=8 then
                begin
                 dictionar_scriere('rom_eng.dat');
                 goto 4;
                end;
       if id=7 then
                begin
                 dictionar_cautare('rom_eng.dat');
                 goto 4;
                end;
       if id=9 then
                begin
                 dictionar_stergere('rom_eng.dat');
                 goto 4;
                end;
      end;
     if id=8 then
      begin
       5:
       curatare;
       cuv[1]:='CAUTARE DE CUVANT';
       cuv[2]:='INTRODUCERE CUVANT';
       cuv[3]:='STERGERE DE CUVANT';
       cuv[4]:='MENIU ANTERIOR';
       meniu(cuv,4,id);
       if id=10 then goto 3;
       if id=8 then
                begin
                 dictionar_scriere('eng_rom.dat');
                 goto 5;
                end;
       if id=7 then
                begin
                 dictionar_cautare('eng_rom.dat');
                 goto 5;
                end;
       if id=9 then
                begin
                 dictionar_stergere('eng_rom.dat');
                 goto 5;
                end;
      end;
    end;
  end;
end.