Program cercuri;
uses crt, graph;
var xr, yr : word;
    xc, yc :integer;
    r : integer;
    i, device, mod_g: integer;
    ch : char;

begin
     clrscr;
     device:= detect;
     initgraph(device, mod_g, 'C:\TP\BGI');
     xc:= getmaxx div 2;
     yc:= getmaxy div 2;
     r:= yc;
     getaspectratio(xr, yr);
     for i:= 1 to 25 do
      begin
           setaspectratio(xr,yr+i*600);
           delay(350);
           circle(xc,yc,r);
      end;
     ch:= readkey;
     cleardevice;
     for i:= 1 to 25 do
      begin
           setaspectratio(xr+i*600,yr);
           delay(350);
           circle(xc,yc,r);
      end;
      ch:= readkey;
      cleardevice;
end.