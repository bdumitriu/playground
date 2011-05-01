uses crt, graph;
var gm,dm,i,k:integer;
begin
dm:=detect;
initgraph(gm,dm,'c:\tp\bgi');
setfillstyle(1,red);
setbkcolor(white);
k:= 100;
for i:=0 to 110 do
begin
fillellipse(320,240,i,i);
sound(k);
delay(100);
nosound;
k:=k+20;
end;
readkey;
closegraph;
end.