uses graph,crt;
var gm,pl,dm,n,i,k,m:integer;
    x:real;
begin
gm:=detect;
initgraph(gm,dm,'c:\tp\bgi');
setbkcolor(white);
setcolor(red);
n:=3;
x:=n*pl/256;
k:=(Getmaxy-100)div 2;
m:=getmaxy div 2;
for i:=0 to 255 do
    begin
    line(m-round(k*sin(x*i)),
    i,i,m-(round(k*cos(x*i))));
    sound(20*i);
    delay(100);
    nosound;
    end;

readln;
closegraph;
end.