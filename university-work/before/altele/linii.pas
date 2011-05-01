Program joc_cu_linii;
uses crt, utilg, graph;
label 1;
var i,j,x,y,z,code:integer;
    c,d,e:char;
    mat:array[1..3,1..5] of 0..1;

Function gata:boolean;
var aux:boolean;
begin
 aux:=true;
 for i:=1 to 3 do
  for j:=1 to 5 do
   if mat[i,j]=1 then aux:=false;
 gata:=aux;
end;

begin
 clrscr;
 writeln;
 ini;
 setcolor(lightblue);
 setfillstyle(solidfill,red);
 bar(0,300,getmaxx,getmaxy);
 line(getmaxx div 2,50,getmaxx div 2,100);
 line(getmaxx div 2 - 20,120,getmaxx div 2 - 20,170);
 line(getmaxx div 2,120,getmaxx div 2,170);
 line(getmaxx div 2 + 20,120,getmaxx div 2 + 20,170);
 for i:=1 to 2 do
  begin
   line(getmaxx div 2 - 20*i,190,getmaxx div 2 - 20*i,240);
   line(getmaxx div 2 + 20*i,190,getmaxx div 2 + 20*i,240);
  end;
 line(getmaxx div 2,190,getmaxx div 2,240);
 settextstyle(1,horizdir,2);
 mat[1,1]:=1;
 for i:=1 to 3 do
  mat[2,i]:=1;
 for i:=1 to 5 do
  mat[3,i]:=1;
 repeat
  1:
  setcolor(lightblue);
  bar(0,300,getmaxx,getmaxy);
  outtextxy(10,320,'Linia de pe care doriti sa stergeti : ');
  c:=readkey;
  outtextxy(380,320,c);
  outtextxy(10,350,'Pozitia de pe care doriti sa incepeti sa stergeti : ');
  d:=readkey;
  outtextxy(520,350,d);
  outtextxy(10,380,'Pozitia unde doriti sa terminati stergerea : ');
  e:=readkey;
  outtextxy(468,380,e);
  val(c,x,code);
  val(d,y,code);
  val(e,z,code);
  setcolor(lightgreen);
  if (x<1) or (x>3) then
                     begin
                      outtextxy(10,420,'Linia trebuie sa fie cuprinsa intre 1 si 3.');
                      readkey;
                      goto 1;
                     end;
  if y>z then
          begin
           outtextxy(10,420,'Pozitia de inceput trebuie sa fie < sau = cu cea de sfarsit.');
           readkey;
           goto 1;
          end;
  case x of
   1:
    begin
     if (y<>1) or (z<>1) then
                          begin
                           outtextxy(10,420,'Pe linia 1 se afla doar o linie.');
                           readkey;
                           goto 1;
                          end;
     if mat[1,1]=0 then
                    begin
                     outtextxy(10,420,'Pozitia a fost deja stearsa.');
                     readkey;
                     goto 1;
                    end;
     mat[1,1]:=0;
     line(getmaxx div 2 - 10,75,getmaxx div 2 + 10,75);
    end;
   2:
    begin
     if (y<1) or (y>3) or (z<1) or (z>3) then
                  begin
                   outtextxy(10,420,'Pe linia 2 se afla doar 3 linii.');
                   readkey;
                   goto 1;
                  end;
     for i:=y to z do
      if mat[2,i]=0 then
                     begin
                      outtextxy(10,420,'Pozitia a fost deja stearsa.');
                      readkey;
                      goto 1;
                     end;
     line(getmaxx div 2 - 30 + 20*(y-1),145,getmaxx div 2 - 30 + 20*z,145);
     for i:=y to z do
      mat[2,i]:=0;
    end;
   3:
    begin
     if (y<1) or (y>5) or (z<1) or (z>5) then
                  begin
                   outtextxy(10,420,'Pe linia 3 se afla doar 5 linii.');
                   readkey;
                   goto 1;
                  end;
     for i:=y to z do
      if mat[3,i]=0 then
                     begin
                      outtextxy(10,420,'Pozitia a fost deja stearsa.');
                      readkey;
                      goto 1;
                     end;
     line(getmaxx div 2 - 50 + 20*(y-1),215,getmaxx div 2 - 50 + 20*z,215);
     for i:=y to z do
      mat[3,i]:=0;
    end;
  end;
 until gata;
 readkey;
 closegraph;
end.