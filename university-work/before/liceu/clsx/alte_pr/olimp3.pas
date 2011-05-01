Program olimpiada_problema_3_cea_cu_pestii;
uses crt;
var mat:array[1..20,1..20] of 0..1;
    i,j,k,n,m,a,b:integer;

Function linie(i:byte):boolean;
var j:byte;
begin
 linie:=true;
 for j:=1 to n do
  if mat[i,j]=1 then linie:=false;
end;

Procedure eliminare(i:byte);
var j,k:byte;
begin
 for j:=i to n-1 do
  for k:=1 to n do
   mat[j,k]:=mat[j+1,k];
 for j:=1 to n-1 do
  for k:=i to n-1 do
   mat[j,k]:=mat[j,k+1];
 n:=n-1;
end;

Function gata:boolean;
var i,j:byte;
begin
 gata:=true;
 for i:=1 to n do
  for j:=1 to n do
   if mat[i,j]=1 then gata:=false;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('.......................................');
 writeln;
 textcolor(lightblue);
 write('Numarul de pesti : ');readln(n);
 write('Introduceti numarul de relatii de mancare : ');readln(m);
 for i:=1 to m do
  begin
   write('Relatia de mancare ',i,' : ');readln(a,b);
   mat[a,b]:=1;
  end;
 writeln;
 textcolor(yellow);
 repeat
  for i:=1 to n do
   if linie(i) then
                begin
                 for k:=1 to n do
                  if mat[k,i]=1 then
                                 begin
                                  writeln('Pestele ',k,' mananca pestele ',i,'.');
                                  k:=n;
                                 end;
                 eliminare(i);
                end;
 until gata;
 if n=1 then writeln('In acvariu a ramas un peste.')
        else writeln('In acvariu au ramas ',n,' pesti.');
 readkey;
end.