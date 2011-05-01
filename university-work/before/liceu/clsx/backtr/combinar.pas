Program de_combinari_de_n_elemente_cate_k;
uses crt;
var n,k,i:integer;
    s:array[1..30] of integer;

Procedure scrie;
var x:integer;
begin
for x:=1 to k do
 write(s[x],' ');
writeln;
end;

Procedure combinari(i:byte);
var j,f:integer;
    cont:boolean;
begin
for j:=1 to n do
 begin
  s[i]:=j;
  cont:=true;
  for f:=1 to i-1 do if s[f]>=s[i] then cont:=false;
  if cont then
   if i=k then scrie
          else combinari(i+1);
 end;
end;

begin
clrscr;writeln;
write('n=');readln(n);
write('k=');readln(k);
combinari(1);
readkey;
end.