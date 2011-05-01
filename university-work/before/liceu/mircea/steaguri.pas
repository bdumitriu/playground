uses crt,graph;
var gm,dm,s,i,j,k:integer;
    co,cond:boolean;
    x:array[1..3]of byte;
begin
dm:=detect;
initgraph(gm,dm,'c:\tp\bgi');
{setbkcolor(15);}
s:=0;
i:=1;
x[i]:=0;
repeat
 while x[i]<6 do
  begin
  x[i]:=x[i]+1;
  cond:=true;
  for j:=1 to i-1 do if x[i]=x[j] then cond:=false;
  if (i=2)and(x[2]<>2) then cond:=false;
  if cond then
    if i=3 then
      begin
      co:=false;
      for k:=1 to 3 do if x[k]=1 then co:=true;
      if co then
        begin
        s:=s+1;
        for k:=1 to 3 do begin
                         if x[k]=1 then setfillstyle(1,2);
                         if x[k]=2 then setfillstyle(1,14);
                         if x[k]=3 then setfillstyle(1,4);
                         if x[k]=4 then setfillstyle(1,15);
                         if x[k]=5 then setfillstyle(1,1);
                         if x[k]=6 then setfillstyle(1,8);
                         bar(40*k,50*s,40*(k+1),45+50*s);
                         end;
        end;
      end
      else begin
           i:=i+1;
           x[i]:=0;
           end;
  end;
i:=i-1;
until i=0;
readln
end.
