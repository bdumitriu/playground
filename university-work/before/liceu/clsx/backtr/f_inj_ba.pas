Program functii_injective;
uses crt;
var s:array[1..100] of integer;
    i,j:byte;
    a,b:string[100];
    cont:boolean;
    m,n:integer;

Procedure scrie;
begin
 writeln('  x: ',a);
 write('f(x): ');
 for j:=1 to m do
  write(b[s[j]],' ');
 writeln;
end;

begin
 clrscr;
 writeln;
 write('Primul sir de caractere : ');readln(a);m:=length(a);
 write('Al doilea sir de caractere : ');readln(b);n:=length(b);
 if m<=n then
  begin
   i:=1;
   s[i]:=0;
   repeat
    while s[i]<n do
     begin
      s[i]:=s[i]+1;
      cont:=true;
      for j:=1 to i-1 do if s[i]=s[j] then cont:=false;
      if cont then
       if i=m then scrie
              else
               begin
                i:=i+1;
                s[i]:=0;
               end;
     end;
    i:=i-1;
   until i=0;
  end;
 if m>n then writeln('Nu exista functii injective.');
 readkey;
end.