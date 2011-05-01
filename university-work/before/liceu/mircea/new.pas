uses graph,crt;
var gm,dm,a,b,s,j,i,k,l,x,y:integer;
begin
dm:=detect;
initgraph(gm,dm,'c:\tp\bgi');
randomize;
repeat
i:=random(640);
j:=random(15);
k:=random(480);
l:=random(100);
x:=random(640);
y:=random(480);
a:=random(11);
b:=random(15);
s:=random(500);
setcolor(j);
setfillstyle(a,b);
pieslice(x,y,i,k,l);
sound(s);
delay(50);
nosound;
until keypressed;
readln;
closegraph;
end.