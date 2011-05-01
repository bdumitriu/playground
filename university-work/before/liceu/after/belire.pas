uses crt;
type pt_a= record
              a, b: integer;
           end;
var i, j: integer;
    a: array [1..100, 1..100] of pt_a;

Function check2(i: integer): boolean;
   var k: integer;

begin
   check2:= true;
   for j:= 1 to 100 do
      if i <> j then
      begin
         for k:= 1 to 100 do
            if (a[i][k].a <> a[j][k].a) or (a[i][k].b <> a[i][k].b) then
               check2:= false;
      end
end;

Function check: boolean;
begin
   check:= false;
   for i:= 1 to 100 do
      if check2(i) then check:= true

end;

begin
   textcolor(lightgray);
   textbackground(black);
   clrscr;
   repeat
   for j:= 1 to 100 do
   begin
      clrscr;
      randomize;
      for i:= 1 to 600 do
      begin
         {a[j][i].a:= random(80);
         a[j][i].b:= random(25);}
         textcolor(random(16));
         gotoxy(random(80), random(25));
         write(#250);
      end;
   end;
   until keypressed;
{   if check = true then    am vrut sa vad daca randomize se repeta
      write('Da')
   else
      write('Nu');            }
   readkey;
end.

