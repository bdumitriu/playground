Program tigri_si_lei;
uses crt;
var l,t,i:integer;
    s:array[0..100] of integer;

Procedure scrie;
var z:integer;
begin
 for z:=1 to l+t do
  write(s[z],' ');
 writeln;
 readln;
end;

Function nr(i:integer):boolean;
var x,q:integer;
begin
 q:=0;
 for x:=1 to i do
  if s[x]=1 then q:=q+1;
 if q=t then nr:=true
        else nr:=false;
end;

Procedure intrare(i:byte);
var j:integer;
    cont:boolean;
begin
 for j:=1 to 2 do                     {1-tigru;2-leu}
  begin
   cont:=true;
   s[i]:=j;
   if (s[i]=1) and (s[i-1]=s[i]) then cont:=false;
   if cont then
    if (i=l+t) and (nr(i)) then scrie
                           else if i<l+t then intrare(i+1);
  end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de lei este ');readln(l);
 write('Numarul de tigri este ');readln(t);
 writeln;
 if t<l then begin
              writeln('Solutiile sunt : ');
              intrare(1);
             end
        else writeln('Nu exista solutii.');
 readkey;
end.