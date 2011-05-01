Program prog1;
uses crt;
var n : integer;
    p, i : longint;
    s1, s2 : string;

Function afl_p(n:integer) : longint;
var i : integer;
    x : longint;
begin
 x:=1;
 for i:=1 to n do
  x:=x*5;
 x:=x-1;
 afl_p:=x;
end;

begin
 clrscr;
 writeln;
 write('Numarul : ');
 readln(n);
 p:=afl_p(n);
 for i:=1 to p do
  begin

 readkey;
end.
