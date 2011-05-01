Program the_25th_of_December_1996;
uses crt, graph, utilg;

Procedure miscarea_tunului;
var c:char;
    x1,x2,x3,i:integer;
begin
 setfillstyle(solidfill,lightgreen);
 setcolor(lightgreen);
 bar((getmaxx div 2)-25,getmaxy-25,(getmaxx div 2)+25,getmaxy-5);
 fillellipse(getmaxx div 2,getmaxy-25,20,10);
 setlinestyle(solidln,0,3);
 line(getmaxx div 2,getmaxy-35,getmaxx div 2, getmaxy-40);
 x1:=(getmaxx div 2)-25;
 x2:=(getmaxx div 2)+25;
 x3:=getmaxx div 2;
 c:=readkey;
 while c<>#27 do
  begin
   if c=',' then
             begin
              setfillstyle(solidfill,black);
              setcolor(black);
              bar(x1,getmaxy-25,x2,getmaxy-5);
              fillellipse(x3,getmaxy-25,20,10);
              line(x3,getmaxy-35,x3, getmaxy-40);
              if x1>0 then
                       begin
                        x1:=x1-10;
                        x2:=x2-10;
                        x3:=x3-10;
                       end;
              setfillstyle(solidfill,lightgreen);
              setcolor(lightgreen);
              bar(x1,getmaxy-25,x2,getmaxy-5);
              fillellipse(x3,getmaxy-25,20,10);
              line(x3,getmaxy-35,x3, getmaxy-40);
             end
            else if c='.' then
                           begin
                            setfillstyle(solidfill,black);
                            setcolor(black);
                            bar(x1,getmaxy-25,x2,getmaxy-5);
                            fillellipse(x3,getmaxy-25,20,10);
                            line(x3,getmaxy-35,x3, getmaxy-40);
                            if x2<getmaxx then
                                           begin
                                            x1:=x1+10;
                                            x2:=x2+10;
                                            x3:=x3+10;
                                           end;
                            setfillstyle(solidfill,lightgreen);
                            setcolor(lightgreen);
                            bar(x1,getmaxy-25,x2,getmaxy-5);
                            fillellipse(x3,getmaxy-25,20,10);
                            line(x3,getmaxy-35,x3, getmaxy-40);
                           end
                          else if c=' ' then
                           begin
                            sound(550);
                            delay(5);
                            nosound;
                            for i:=getmaxy-45 downto 10 do
                             begin
                              setcolor(lightgreen);
                              line(x3,i,x3,i-10);
                              delay(1);
                              setcolor(black);
                              line(x3,i,x3,i-10);
                             end;
                           end;
   c:=readkey;
  end;
 end;

begin
 ini;
 miscarea_tunului;
end.