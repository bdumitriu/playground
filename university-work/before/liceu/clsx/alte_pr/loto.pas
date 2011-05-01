Program tragere_loto;
uses crt;
label 1;
var i,j:byte;
    a:array[1..6] of byte;
    c:char;

Function ok(j:byte):boolean;
var x:byte;
begin
 ok:=true;
 for x:=1 to j-1 do
  if i=a[x] then ok:=false;
end;

begin
 clrscr;
 writeln;
 j:=0;
 repeat
  for i:=1 to 49 do
   begin
    c:=readkey;
    if c='l'  then
                   begin
                    j:=j+1;
                    if ok(j) then a[j]:=i
                             else j:=j-1;
                    if j=6 then goto 1;
                   end;
   end;
 until j=10000;
1: for i:=1 to 6 do
  write(a[i],' ');
 writeln;
 readkey;
end.