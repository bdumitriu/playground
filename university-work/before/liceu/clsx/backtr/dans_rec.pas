Program petrecere;
uses crt;
var m,n,q:integer;                  {m-nr fete,n-nr baieti}
    x:array[1..100] of integer;
Procedure scrie;
var y:byte;
begin
writeln('Solutia ',q,' :');
q:=q+1;
for y:=1 to m do
 writeln('fata ',y,' cu baiatul ',x[y]);
readln;
end;
Procedure dans(i:byte);
var j,k:byte;
    cont:boolean;
begin
for j:=1 to n do
 begin
  x[i]:=j;
  cont:=true;
  for k:=1 to i-1 do if x[k]=x[i] then cont:=false;
  if cont then
   if i=m then scrie
          else dans(i+1);
  end;
end;
begin
clrscr;
write('nr de fete : ');readln(m);
write('nr de baieti : ');readln(n);
q:=1;
if m>n then writeln('mai aduceti baieti!')
       else dans(1);
readkey;
end.

