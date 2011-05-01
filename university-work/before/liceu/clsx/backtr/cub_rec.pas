Program turnuri_de_cuburi;
uses crt;
type cub=record
         latura:integer;
         culoare:string;
         end;
var c:array[1..100] of cub;
    n,i,z:integer;
    s:array[1..100] of integer;

Procedure scrie;
var j:integer;
begin
 for j:=1 to z do
  write(s[j],' ');
 readln;
end;

Function ok(i:byte):boolean;
var j:integer;
begin
 ok:=true;
 if i>1 then
  if (c[s[i]].latura>c[s[i-1]].latura) or (c[s[i]].culoare=c[s[i-1]].culoare) then
                                                                   ok:=false;
 for j:=1 to i-1 do
  if s[i]=s[j] then ok:=false;
end;

begin
 clrscr;
 writeln;
 write('Numarul de cuburi : ');readln(n);
 for i:=1 to n do
  with c[i] do
   begin
    write('Culoarea cubului ',i,' este : ');readln(culoare);
    write('Lungimea laturii cubului ',i,' este : ');readln(latura);
   end;
  write('Numarul de cuburi din turn este : ');readln(z);
 i:=1;
 s[i]:=0;
 repeat
  while s[i]<n do
   begin
    s[i]:=s[i]+1;
    if ok(i) then
     if i=z then scrie
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