uses crt,graph;
type reper=^depou;
     depou=record
           cod:char;
           leg:reper;
           end;
var prim,ultim,p:reper;
    c:char;
    gm,dm:integer;

procedure depo;
var k:integer;
begin
setfillstyle(1,10);
bar(1,358,getmaxx-1,getmaxy-1);
setfillstyle(1,yellow);
bar(5,230,getmaxx-5,380);
setcolor(7);
rectangle(5,230,getmaxx-5,380);
setfillstyle(1,7);
bar(3,180,getmaxx-2,230);
setcolor(0);
for k:=1 to 19 do line(k*38,180,k*38,230);
outtextxy(10,240,'Ü Iesire');
outtextxy(10,248,'ßßßßßßßß');
outtextxy(557,240,'Intrare Ü');
outtextxy(557,248,'ßßßßßßßßß');
setlinestyle(0,0,3);
setcolor(brown);
for k:=0 to 63 do line(k*10+3,353,k*10,358);
setlinestyle(0,0,1);
setcolor(0);
line(1,354,getmaxx-1,354);
line(1,357,getmaxx-1,357);
end;

procedure locomo(x,y:word);
var l:byte;
begin
setfillstyle(1,15);setcolor(0);
for l:=1 to 4 do fillellipse(x+l*16-1,347,6,6);
setfillstyle(1,8);
bar(x,300,y,350);
setfillstyle(1,red);setcolor(0);
for l:=1 to 4 do fillellipse(x+l*16-7,350,6,6);
line(x+9,347,x+57,347);
setfillstyle(1,0);
bar(x+10,280,x+20,310);
bar(x+8,278,x+22,280);
bar(x-2,345,x+1,347);
fillellipse(x-3,346,1,3);
bar(y-1,345,y+2,347);
fillellipse(y+3,346,1,3);
setfillstyle(1,yellow);
bar(y-25,310,y-10,328);
end;

procedure listare;
var p:reper;
    i,j,k:longint;
begin
if prim=nil then begin
                 depo;
                 setcolor(yellow);
                 outtextxy(7,40,'Nu sunt locomotive in depou ! ');
                 end
            else begin
                 depo;i:=-70;j:=0;
                 p:=prim;
                 repeat
                 i:=i+80;j:=j+80;
                 locomo(i,j);
                 setcolor(13);
                 settextstyle(1,0,1);
                 outtextxy(i+17,313,p^.cod);
                 settextstyle(0,0,0);
                 p:=p^.leg;
                 until p=nil;
                 end;
setcolor(yellow);
outtextxy(7,60,'... apasati ENTER pentru a continua.');
readln;
end;

procedure intrare;
var p:reper;
begin
new(p);
setcolor(yellow);
outtextxy(7,40,'Codul locomotivei :');
p^.cod:=readkey;
outtextxy(165,40,p^.cod);delay(500);
p^.leg:=nil;
if prim=nil then prim:=p
            else ultim^.leg:=p;
ultim:=p;
end;

procedure iesire;
var p:reper;
begin
if prim= nil then outtextxy(7,40,'Nu sunt locomotive in depou !')
             else begin
                  p:=prim;
                  prim:=p^.leg;
                  outtextxy(7,40,'Iese locomotiva cu codul : ');outtextxy(220,40,p^.cod);
                  dispose(p);
                  end;
delay(900);
end;

begin
clrscr;
prim:=nil;
dm:=detect;
initgraph(gm,dm,'c:\tp\bgi');
setfillstyle(1,1);
bar(1,1,getmaxx-1,getmaxy-1);
setcolor(15);
rectangle(0,0,getmaxx,getmaxy);
rectangle(3,3,getmaxx-3,100);
setcolor(yellow);
outtextxy(7,7,'Comenzi : i - intrare   e - iesire   l - listare   s - sfarsitul programului');
outtextxy(7,20,'Comanda : ');
c:=readkey;
outtextxy(80,20,c);
while c<>'s' do
begin
if c='i' then intrare
         else if c='e' then iesire
                       else if c='l' then listare
                                     else begin
                                          outtextxy(7,40,'Nu este nici o comanda controlata de aceasta tasta !');
                                          outtextxy(7,60,'Va rugam sa recititi primul rand afisat pe monitor.');
                                          outtextxy(345,80,'... apasati ENTER pentru a continua.');
                                          readln;
                                          end;
setfillstyle(1,1);
bar(1,30,getmaxx-1,getmaxy-1);
bar(80,13,90,30);
setcolor(15);
rectangle(3,3,getmaxx-3,90);
setcolor(yellow);
c:=readkey;
outtextxy(80,20,c);
end;
listare;
end.