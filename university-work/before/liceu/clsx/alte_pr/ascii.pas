Program ascii;
uses crt;
var I:integer;
begin
 clrscr;
 for i:=1 to 255 do
  begin
   writeln(chr(i),'= Alt + ',i);
   readkey;
  end;
end.