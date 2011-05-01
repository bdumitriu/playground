Program functii_injective;
uses crt;
var i,a,b:integer;
    m,n:array[1..100] of char;
    s:array[1..2] of char;
    q:array[1..2] of string;

Procedure scrie;
var x:integer;
begin
 for x:=1 to i do
  if x=1 then write(s[x],'->')
         else writeln(s[x]);
end;

Procedure funct(i:byte);
var j:integer;
begin
 for j:=1 to length(q[i]) do
  begin
   s[i]:=n[j];
   if i=2 then scrie
          else funct(i+1);
  end;
end;

begin
 clrscr;
 writeln;
 write('Domeniul functiei este : ');readln(q[1]);
 write('Codomeniu functiei este : ');readln(q[2]);
 for i:=1 to length(q[1]) do
  n[i]:=q[1][i];
 for i:=1 to length(q[2]) do
  m[i]:=q[1][i];
 funct(1);
 readkey;
end.