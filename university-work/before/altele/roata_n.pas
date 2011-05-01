Program Roata_Norocului;
uses crt, graph, utilg;

Procedure ecran;
var i,j:integer;
begin
 setcolor(white);
 line(0,300,getmaxx,300);
 line(0,320,getmaxx,320);
 settextstyle(0,horizdir,1);
 setcolor(brown);
 outtextxy(1,310,'F1-invarte roata');
 setcolor(blue);
 for i:=80 to 560 do
  if i mod 40 = 0 then line(i,50,i,200);
 line(80,50,560,50);
 line(80,100,560,100);
 line(80,150,560,150);
 line(80,200,560,200);
 settextstyle(1,horizdir,4);
 setcolor(red);
 outtextxy(175,5,'Roata NOROCULUI');
end;

{Procedure roata;}

begin
 ini;
 setbkcolor(lightgreen);
 ecran;
 readkey;
end.