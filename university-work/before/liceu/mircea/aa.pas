uses graph,crt;
var gm,dm,a,b:integer;
begin
dm:=detect;
initgraph(gm,dm,'c:\tp\bgi');
setbkcolor(15);
setcolor(8);
setlinestyle(0,0,3);
circle(315,83,20);
setlinestyle(0,0,3);
moveto(330,98);
lineto(330,110);
setlinestyle(0,0,3);
lineto(290,155);
lineto(296,88);
lineto(300,155);
lineto(330,111);
arc(230,170,344,30,120);
arc(240,185,350,40,120);
moveto(358,208);
lineto(290,158);
lineto(288,220);
setlinestyle(0,0,3);
lineto(279,220);
lineto(291,220);
setlinestyle(0,0,3);
lineto(298,159);
lineto(345,204);
for a:=0 to 9 do begin
 settextstyle(a,0,5);
outtextxy(0,40*a,'Ganditorul...');
sound(a*80);
delay(100);
nosound;
end;
readln;
closegraph;
end.