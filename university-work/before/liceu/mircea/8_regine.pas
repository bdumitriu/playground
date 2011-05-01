program problema_celor_8_regine;
uses crt,graph;
type vect=array[1..8]of integer;
var gm,dm,i,j,n:integer;
    r:vect;
    nu_se_ataca:boolean;

procedure WaitToGo;
var Ch:char;
begin
  repeat until KeyPressed;
  Ch:=ReadKey;
  if Ch=#0 then ch:=readkey;
  if Ch=#27 then Halt(0)
            else ClearDevice;
end;{Aceasta procedura a fost preluata din BGIDEMO.PAS}

begin
clrscr;
dm:=detect;
initgraph(gm,dm,'c:\tp\bgi');
i:=1;
r[i]:=0;
n:=0;
 repeat
  while r[i]<=7 do
   begin r[i]:=r[i]+1;
         nu_se_ataca:=true;
         for j:=1 to i-1 do
          if (r[i]=r[j]) or (abs(i-j)=abs(r[j]-r[i]))
           then nu_se_ataca:=false;
         if nu_se_ataca then if i=8 then
           begin
               setcolor(15);
               settextstyle(4,0,2);
               outtextxy(100,75,'Problema celor opt regine');
               settextstyle(11,0,1);
               outtextxy(40,472,'Apasati ENTER pentu a continua sau ESC pentru a iesi din program.');
               setcolor(15);
               setlinestyle(0,0,1);
               rectangle(140,220,300,380);
               rectangle(160,220,280,380);
               rectangle(180,220,260,380);
               rectangle(200,220,240,380);
               rectangle(220,220,220,380);
               rectangle(140,240,300,360);
               rectangle(140,260,300,340);
               rectangle(140,280,300,320);
               rectangle(140,300,300,300);
               setcolor(14);
               setfillstyle(1,14);
               for i:=1 to 8 do
               for j:=1 to 8 do if j=r[i] then
                        fillellipse(130+20*i,10+200+20*j,3,6);
               waittogo;
               setfillstyle(1,black);
               bar(140,220,300,380);
           end
              else begin
                        i:=i+1;
                        r[i]:=0;
                   end;
  end;
 i:=i-1;
 until i=0;
end.






