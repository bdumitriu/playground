uses graph,crt;
const
  Triangle: array[1..3] of PointType = ((X:  250; Y: 300),
    (X: 320; Y: 280), (X: 390; Y: 300));
var dm,gm,i,j,k:integer;

procedure geam(i,j:longint);
begin
setlinestyle(0,0,1);
setcolor(0);
setfillstyle(1,7);
bar(i,j,i+16,j+16);
rectangle(i,j,i+16,j+16);
line(i,j,i+16,j+16);
line(i+16,j,i,j+16);
setfillstyle(1,yellow);
bar(i+4,j+4,i+12,j+12);
rectangle(i+3,j+3,i+13,j+13);
end;

begin
dm:=detect;
initgraph(gm,dm,'c:\tp\bgi');
setfillstyle(1,0);
bar(0,0,getmaxx,getmaxy div 2);
setfillstyle(1,8);
bar(0,getmaxy div 2,getmaxx,getmaxy);
setcolor(0);
setfillstyle(1,7);
bar(220,220,380,346);
rectangle(220,220,380,346);
setfillstyle(1,7);
bar(200,170,220,350);
bar(360,170,380,350);
rectangle(200,170,220,350);
rectangle(360,170,380,350);
bar3d(195,130,225,170,5,topon);
bar3d(355,130,385,170,5,topon);
rectangle(195,130,225,170);
rectangle(355,130,385,170);
geam(202,140);
geam(362,140);
setcolor(red);
for j:=190 to 230 do line(210,100,j,130);
for j:=350 to 390 do line(370,100,j,130);
setfillstyle(4,brown);
setcolor(brown);
bar(230,220,350,345);
rectangle(230,220,350,345);
fillellipse(290,220,60,30);
ellipse(290,220,0,360,60,30);
bar(231,221,349,344);
setcolor(0);
setlinestyle(0,0,3);
line(289,191,289,345);
line(292,191,292,345);
setlinestyle(3,0,1);setcolor(8);
for j:=221 to 240 do line(101,j,141,j-60);
setlinestyle(0,0,1);
for j:=240 to 343 do line(101,j,141,j-60);
for j:=5 to 9 do
begin
setfillstyle(1,7);
bar(j*20,220,j*20+10,345);
bar(j*20+10,240,j*20+20,345);
end;
for j:=19 to 23 do
begin
bar(j*20+10,220,j*20+20,345);
bar(j*20,240,j*20+10,345);
end;
setcolor(7);setlinestyle(3,0,1);
for j:=222 to 240 do line(480,j,520,j-60);
setlinestyle(0,0,1);
for j:=240 to 343 do line(480,j,520,j-60);
setlinestyle(0,0,1);
setcolor(0);
rectangle(200,170,220,350);
rectangle(360,170,380,350);
line(480,220,480,345);
for j:=5 to 5 do
begin
for i:=0 to getmaxx div 2 do
begin
setcolor(j);
rectangle(getmaxx div 2-i,getmaxy div 2-i,getmaxx div 2+i,getmaxy div 2+i);
end;
setcolor(j-1);
settextstyle(4,0,4);
outtextxy(111,111,'Liceul Teoretic Gheorge Sincai');
for i:=getmaxx div 2 downto 0 do
begin
setcolor(j+2);
rectangle(getmaxx div 2-i,getmaxy div 2-i,getmaxx div 2+i,getmaxy div 2+i);
end;
end;
setcolor(yellow);
setlinestyle(0,2,1);
setfillstyle(1,yellow);
bar(40,300,150,400);
bar(490,300,600,400);
bar(150,300,490,380);
setcolor(0);
rectangle(40,300,150,400);
rectangle(490,300,600,400);
rectangle(150,300,490,380);
setcolor(yellow);
bar(260,300,380,410);
  if GraphResult <> grOk then
    Halt(1);
  FillPoly(SizeOf(Triangle) div SizeOf(PointType), Triangle);
setcolor(0);
rectangle(260,300,380,410);
moveto(320,280);
lineto(390,300);
lineto(250,300);
lineto(320,280);
setcolor(red);
readln;
closegraph;
end.