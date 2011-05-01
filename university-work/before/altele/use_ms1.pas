Program de_folosire_a_unitului_MOUSE;
uses Crt, Graph, Mouse;
var  gd, gm, k, x, y, oldx, oldy : integer;
     s1, s2 : String;
begin
   ResetMouse;
   if not MouseExist then Halt;
   gd := Detect;
   initgraph(gd, gm, 'C:\TP\BGI');
   if GraphResult <> grOK then
      Halt;

   DefMouseRange(50, 300, 50, 200);
   SetColor(Blue);
   Rectangle(50, 50, 300, 200);
   SetFillStyle(SolidFill, LightGray);
   bar(390, 10, 500, 30);
   ShowMouse;
   DefMouseShape(Cross);
   oldx := 0;
   oldy := 0;
   MouseMove(50, 50);
   repeat
      k := GetMouse(x, y);
      if (x <> oldx) or (y <> oldy) then
      begin
         oldx := x;
         oldy := y;
         Str(x, s1);
         Str(y, s2);
         HideMouse;
         SetFillStyle(SolidFill, LightGray);
         bar(390, 10, 500, 30);
         setColor(Black);
         outtextxy(400, 20, s1+','+s2);
         ShowMouse;
      end;
   until k and 2 <> 0;  { Asteapta apasarea butonului dreapta
                                             al mouse-ului }
   closegraph;
end.
