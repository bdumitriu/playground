{$N+}
Program alb_negru;
uses crt, graph;
var f: text;
    x1, x2, y1, y2: array[1..100] of integer;
    i, j, drv, mod_g: integer;
    a1, a2, a3, a4: ;

begin
   clrscr;
   writeln;
   assign(f, 'gr.in');
   reset(f);
   i:= 0;
   while not(eof(f)) do
   begin
      inc(i);
      readln(f, x1[i], y1[i], x2[i], y2[i]);
   end;
   drv:= detect;
   initgraph(drv, mod_g, 'c:\tp\bgi');
   line(x1[1], y1[1], x2[1], y2[1]);
   setcolor(blue);
   i:= 1;
   a1:= (y2[i]-y1[i])/(x2[i]-x1[i]);
   {a2:= (x1[i]*y2[i]-y1[i]*x2[i])/(x2[i]-x1[i]);}
   a2:= x1[i]*y2[i];
   a3:= y1[i]*x2[i];
   a4:= x2[i]-x1[i];
   a2:= a2-a3;
   a2:= a2/a4;

   readkey;
end.