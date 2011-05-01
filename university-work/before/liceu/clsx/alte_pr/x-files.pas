Program The_X_Files_Theme;
uses crt,utilg,graph;
type sir=array[1..24] of word;
const frecv:sir=(220,330,294,330,392,330,220,330,294,330,440,330,
                 524,494,440,392,440,330,524,494,440,392,440,330);
      p:word=1200;
      r:word=150;
var i:byte;
begin
 clrscr;
 ini;
 settextstyle(sansseriffont,horizdir,5);
 repeat
  for i:=1 to 24 do
   begin
    sound(frecv[i]);
     if i in [13,19,1,7] then
                          delay(1090)
                         else
                          delay(1000);
    nosound;
    if i in [1,2,3,4,5,7,8,9,10,11,13,14,15,16,17,19,20,21,22,23] then
     begin
      setcolor(random(15));cleardevice;
      outtextxy(random(300),random(getmaxy-50),'The X-Files Theme');
      delay(r);
     end;
    if i in [6,12,18,24] then
     begin
      delay(p);
      setcolor(random(15));cleardevice;
      outtextxy(random(300),random(getmaxy-50),'The X-Files Theme');
     end;
   end;
 until keypressed;
end.