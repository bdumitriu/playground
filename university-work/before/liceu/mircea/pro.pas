uses crt,graph;
var gm,dm,i,j:integer;
    x:array[1..4]of integer;
begin
dm:=detect;
initgraph(dm,gm,'c:\tp\bgi');
setcolor(7);
settextstyle(0,0,17);
outtextxy(100,100,'PRO');
for i:=100 to 500 do for j:=240 to 250 do putpixel(i,j,green);
for i:=100 to 500 do for j:=255 to 265 do putpixel(i,j,red);
for i:=100 to 500 do for j:=270 to 280 do putpixel(i,j,blue);
settextstyle(0,0,9);
outtextxy(230,230,'TV');
delay(20000);
cleardevice;
settextstyle(0,0,2);
outtextxy(150,200,'Te uiti si castigi !');
delay(20000);
cleardevice;
setcolor(white);
settextstyle(2,0,9);
outtextxy(180,200,'Esti pe PRO TV');
delay(20000);
cleardevice;
outtextxy(220,200,'Foarte bine');
delay(20000);
cleardevice;
outtextxy(140,200,'Asta-i televiziunea !');
delay(20000);
closegraph;
end.