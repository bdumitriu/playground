Program de_generare_a_produsului_cartezian_recursiv;
uses crt;
var c:array[1..20] of integer;
    n,m:integer;

Procedure scrie;
var j:integer;
begin
for j:=1 to m do
 write(c[j],' ');
writeln;
end;

Procedure cartezian(i:byte);
var j:integer;
begin
for j:=1 to n do
 begin
  c[i]:=j;
  if i<m then cartezian(i+1)
         else scrie;
 end;
end;

begin
clrscr;
writeln;
write('Cate elemente are multimea ? ');readln(n);
write('La ce putere se ridica multimea ? ');readln(m);
cartezian(1);
readkey;
end.