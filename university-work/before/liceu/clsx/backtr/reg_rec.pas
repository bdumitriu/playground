Program regine_recursiv;
uses crt;
var s:array[1..8] of 1..8;
    n:byte;

Function nu_se_ataca(i:byte):boolean;
var j:byte;
begin
 nu_se_ataca:=true;
 for j:=1 to i-1 do
 if (s[i]=s[j]) or (abs(s[i]-s[j])=i-j) then nu_se_ataca:=false;
end;

Procedure scrie;
var x,y:byte;
begin
 n:=n+1;
 write('Solutia ',n,':');
 for x:=1 to 8 do
  begin
   writeln;
   for y:=1 to 8 do
    if y=s[x] then write('R')
              else write('*');
   end;
   writeln;
   readln;
end;

Procedure reg(i:byte);
var j:byte;
    cont:boolean;
begin
 for j:=1 to 8 do
  begin
   cont:=true;
   s[i]:=j;
   if nu_se_ataca(i)=false then cont:=false;
   if cont then
    if i=8 then scrie
           else reg(i+1);
  end;
end;

begin
 clrscr;
 writeln;
 n:=0;
 reg(1);
end.