Program steaguri_prin_backtracking_nerecursiv;
uses crt;
label 1;
var s:array[1..3] of integer;
    i,j:byte;
    cont:boolean;

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

begin
clrscr;
writeln;
i:=1;
s[i]:=0;
repeat
while s[i]<6 do
 begin
  s[i]:=s[i]+1;
  cont:=true;
  if i=3 then for j:=1 to 3 do if s[j]=5 then
                                         begin
                                          cont:=true;
                                          goto 1;
                                         end
                                        else cont:=false;
 1:if (s[i]=1) and (i<>2) then cont:=false;
 for j:=1 to i-1 do if s[i]=s[j] then cont:=false;
 if cont then
  if i=3 then scrie
         else
          begin
           i:=i+1;
           s[i]:=0;
          end;
  end;
 i:=i-1;
until i=0;
readkey;
end.