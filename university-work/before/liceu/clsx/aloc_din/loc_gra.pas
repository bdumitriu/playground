Program locomotive_cu_coada;
uses crt,utilg,graph;
type reper=^locom;
     locom=record
            leg:reper;
            cod:char;
           end;
var prim,ultim:reper;
    c:char;
    i:word;

Procedure ecran_principal;
var i:byte;
begin
 setbkcolor(green);
 setcolor(red);
 line(5,5,5,getmaxy-5);
 line(5,5,getmaxx-5,5);
 line(getmaxx-5,5,getmaxx-5,getmaxy-5);
 line(getmaxx-5,getmaxy-5,5,getmaxy-5);
 setcolor(white);
 settextstyle(0,horizdir,2);
 outtextxy(180,20,'Lista de comenzi');
 settextstyle(0,horizdir,1);
 outtextxy(95,50,'i-iesire  e-iesire  l-listare  s-sfarsitul programului');
 setcolor(red);
 line(5,60,getmaxx-5,60);
 for i:=100 to 120 do
  line(5,i,getmaxx-5,i);
end;

Procedure sina;
var i,j:0..700;
begin
 setcolor(blue);
 for i:=290 to 292 do
  line(10,i,getmaxx-10,i);
 for i:=310 to 312 do
  line(10,i,getmaxx-10,i);
 for i:=10 to getmaxx-10 do
  if i mod 30 = 0 then
                   for j:=i to i+5 do
                    line(j,287,j-20,315);
end;

Procedure vagon(x:byte;p:reper);
begin
 setfillstyle(solidfill,yellow);
 bar3d(x*100+20,245,x*100+115,300,10,topon);
 setfillstyle(solidfill,lightred);
 fillellipse(x*100+40,300,12,12);
 fillellipse(x*100+95,300,12,12);
 setcolor(lightblue);
 outtextxy(x*100+35,260,p^.cod);
 setcolor(white);
end;

Procedure intrare;
var p:reper;
    x:char;
begin
 outtextxy(70,80,'The code of the locomotive you want to enter in the depot : ');
 x:=readkey;
 outtextxy(550,80,x);
 if prim=nil then
              begin
               new(p);
               p^.cod:=x;
               p^.leg:=nil;
               prim:=p;
               ultim:=p;
              end
             else
              begin
               new(p);
               p^.cod:=x;
               p^.leg:=nil;
               ultim^.leg:=p;
               ultim:=p;
              end;
end;

Procedure iesire;
var p:reper;
begin
 if prim=nil then
              begin
               outtextxy(70,80,'There are no locomotives in the depot.');
               readkey;
              end
             else
              begin
               p:=prim;
               prim:=prim^.leg;
               outtextxy(70,80,'The code of the locomotive that is taken out is ');
               outtextxy(457,80,p^.cod);
               dispose(p);
               readkey;
              end;
end;

Procedure listare;
var p:reper;
    x:shortint;
begin
 if prim=nil then
              begin
               outtextxy(70,80,'There are no locomotives in the depot.');
               readkey;
              end
             else
              begin
               p:=prim;
               x:=-1;
               repeat
                x:=x+1;
                vagon(x,p);
                p:=p^.leg;
               until p=nil;
               outtextxy(250,400,'Press any key to continue');
               readkey;
               setfillstyle(solidfill,green);
               bar(10,200,getmaxx-10,430);
               sina;
              end;
end;

begin
 ini;
 ecran_principal;
 sina;
 prim:=nil;
 repeat
 setcolor(white);
 outtextxy(70,70,'Your order, SIR : ');
 c:=readkey;
 outtextxy(220,70,c);
  case c of
   'S','s','L','l':listare;
   'I','i':intrare;
   'E','e':iesire;
  end;
 delay(300);
 setfillstyle(solidfill,green);
 bar(220,70,270,80);
 bar(70,80,600,90);
 until (c='S') or (c='s');
 readkey;
end.