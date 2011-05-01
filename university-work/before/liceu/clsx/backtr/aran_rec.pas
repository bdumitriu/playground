Program aranjare;
uses crt;
var s:array[1..100] of integer;
    n,m:integer;

Procedure scrie;
var j:byte;
begin
 for j:=1 to m-1 do
  write(s[j],',');
 write(s[m],'.');
 readln;
end;

Procedure aranj(i:byte);
var j:byte;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if i=m then scrie
          else aranj(i+1);
  end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de elemente din multime este ');readln(n);
 write('Numarul de aranjari ');readln(m);
 aranj(1);
 readkey;
end.