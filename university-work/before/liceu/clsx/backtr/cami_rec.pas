Program camile;
uses crt;
var s:array[1..100] of integer;
    i,n:integer;

Procedure scrie;
var x:integer;
begin
 for x:=1 to n do
  write(s[x],' ');
 readln;
end;

Function ok(i:byte):boolean;
var x:integer;
begin
 ok:=true;
 if i>1 then if s[i-1]+1=s[i] then ok:=false;
 for x:=1 to i-1 do if s[x]=s[i] then ok:=false;
end;

Procedure aranjare(i:byte);
var j:integer;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if ok(i) then
    if i=n then scrie
           else aranjare(i+1);
  end;
end;

begin
 clrscr;
 writeln;
 writeln('Acesta este un program care daca ii veti da un numar de camile ');
 writeln('vi le va rearanja pe acestea in toate modurile posibile astfel ');
 writeln('incat nici una din camile sa nu fie in spatele celei in care a ');
 writeln('s-a aflat in pozitia initiala (camila 1, camila 2,..., camila n).');
 writeln;
 write('Numarul de camile : ');readln(n);
 aranjare(1);
 readln;
end.