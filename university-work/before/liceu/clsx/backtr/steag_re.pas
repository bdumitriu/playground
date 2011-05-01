Program steaguri;
uses crt;
{Culorile : 1-galben, 2-albastru, 3-alb, 4-rosu, 5-verde, 6-negru}
var s:array[1..3] of integer;
    i:byte;

Procedure scrie;
begin
 for i:=1 to 3 do
  case s[i] of
   1:write(' galben');
   2:write(' albastru');
   3:write(' alb');
   4:write(' rosu');
   5:write(' verde');
   6:write(' negru');
  end;
 readln;
end;

Function ok(i:integer):boolean;
label 1;
var k:byte;
begin
 ok:=true;
 if i=3 then for k:=1 to 3 do if s[k]=5 then
                                         begin
                                          ok:=true;
                                          goto 1;
                                         end
                                        else ok:=false;
 1:if (s[i]=1) and (i<>2) then ok:=false;
 for k:=1 to i-1 do if s[i]=s[k] then ok:=false;
end;

Procedure culori(i:byte);
var j:byte;
begin
 for j:=1 to 6  do
 begin
  s[i]:=j;
  if ok(i) then
   if i=3 then scrie
          else culori(i+1);
 end;
end;

begin
 clrscr;
 writeln;
 culori(1);
 readkey;
end.