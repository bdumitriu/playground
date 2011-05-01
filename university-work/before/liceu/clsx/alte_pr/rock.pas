Program proiect_pentru_ora_de_informatica;
uses crt, graph, utilg;
var driver,mod_g,i:integer;
    pal:palettetype;
    key,key2:char;

Procedure schimbarea_paletei;
 begin
  for i:=0 to 15 do setpalette(i,i);
  for i:=0 to 15 do setrgbpalette(i,0,0,4*i);
 end;

Procedure desen_promo;
 begin
  for i:=0 to 15 do
   begin
    setfillstyle(solidfill,i);
    bar(0,(i*(getmaxy div 14)-(getmaxy div 14)),getmaxx,(i*(getmaxy div 14)));
   end;
  setcolor(0);
  settextstyle(sansseriffont,horizdir,1);
  outtextxy(190,getmaxy-25,'Copyright (C) Bogdan Dumitriu,Cluj-Napoca 1996');
  settextstyle(triplexfont,horizdir,7);
  delay(2000);
  outtextxy(160,100,'PROGRAM');
  sound(250);
  delay(50);
  nosound;
  delay(1500);
  outtextxy(70,170,'DE');
  sound(250);
  delay(50);
  nosound;
  delay(1500);
  outtextxy(180,170,'INFORMATICA');
  sound(250);
  delay(50);
  nosound;
  settextstyle(sansseriffont,horizdir,1);
  delay(2000);
  outtextxy(300,280,'Apasati orice tasta pentru continuare.');
  sound(1000);
  delay(50);
  nosound;
  readkey;
 end;

Procedure meniu;
begin
 {for i:=0 to 63 do
  begin
   setfillstyle(solidfill,yellow);
   setrgbpalette(yellow,i,i,0);
   bar(0,0,getmaxx div 2, getmaxy div 2);
   setfillstyle(solidfill,blue);
   setrgbpalette(blue,0,0,i);
   bar(getmaxx div 2,0,getmaxx,getmaxy div 2);
   setfillstyle(solidfill,red);
   setrgbpalette(red,i,0,0);
   bar(0,getmaxy div 2,getmaxx div 2,getmaxy);
   setfillstyle(solidfill,green);
   setrgbpalette(green,0,i,0);
   bar(getmaxx div 2,getmaxy div 2,getmaxx,getmaxy);
  end;
 setrgbpalette(yellow,31,31,0);
 setrgbpalette(blue,0,0,31);
 setrgbpalette(red,31,0,0);
 setrgbpalette(green,0,31,0);
 setfillstyle(1,0);
 bar(0,0,getmaxx,getmaxy);}
 setbkcolor(black);
 settextstyle(sansseriffont,horizdir,2);
 for i:=150 to 190 do
  begin
   setcolor(red);
   outtextxy(i+1,100,'1.  Linie 1');
   setcolor(lightgreen);
   outtextxy(385-i,200,'2.  Linie 2');
   setcolor(yellow);
   outtextxy(i+7,300,'3.  Linie 3');
   if i<190 then
    begin
     setcolor(black);
     outtextxy(i+1,100,'1.  Linie 1');
     outtextxy(385-i,200,'2.  Linie 2');
     outtextxy(i+7,300,'3.  Linie 3');
    end;
  end;
 delay(1000);
 setcolor(lightcyan);
 outtextxy(10,400,'Apasati 1,2 sau 3 pentru a executa programele respective.');
 delay(500);
 outtextxy(120,430,'Apasati orice alta tasta pentru iesire.');
end;

Procedure primirea_comenzii;
label 1,2,3;
begin
 3:key:=readkey;
 if key='1' then
  begin
   cleardevice;
   line(0,0,100,100);
   outtextxy(10,420,'Apasati orice tasta pentru iesire sau m pt. revenire la meniu.');
   key2:=readkey;
   if key2='m' then goto 1
               else goto 2;
  end;
 if key='2' then
  begin
   cleardevice;
   line(100,100,200,200);
   outtextxy(110,420,'Apasati orice tasta pentru iesire.');
  end;
 if key='3' then
  begin
   cleardevice;
   line(200,200,300,300);
   outtextxy(110,420,'Apasati orice tasta pentru iesire.');
  end;
 1:cleardevice;
 setbkcolor(black);
 settextstyle(sansseriffont,horizdir,2);
 setcolor(red);
 outtextxy(191,100,'1.  Linie 1');
 setcolor(lightgreen);
 outtextxy(191,200,'2.  Linie 2');
 setcolor(yellow);
 outtextxy(191,300,'3.  Linie 3');
 delay(1000);
 outtextxy(10,400,'Apasati 1,2 sau 3 pentru a executa programele respective.');
 delay(500);
 outtextxy(120,430,'Apasati orice alta tasta pentru iesire.');
 goto 3;
 2:
end;

begin
ini;
schimbarea_paletei;
desen_promo;
cleardevice;
rev;
meniu;
{readkey;}
readkey;
primirea_comenzii;
end.