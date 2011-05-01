Program asfaltare_drumuri;
uses crt;
var s:array[1..20] of integer;
    m,n,i,j,k:integer;
    mat:array[1..5,1..5] of integer;
    nr_sol:integer;

Procedure scrie;
var x:integer;
begin
 nr_sol:=nr_sol+1;
 writeln('Solutia ',nr_sol,' : ');
 for x:=1 to m+1 do
  write(s[x],' ');
 readln;
end;

Procedure asfalt(i:byte);
var j:byte;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if ((i>1) and (mat[s[i-1],s[i]]=1)) or (i=1) then
    begin
     if i>1 then
             begin
              mat[s[i-1],s[i]]:=0;
              mat[s[i],s[i-1]]:=0;
             end;
     if i=m+1 then scrie
              else asfalt(i+1);
     if i>1 then
             begin
              mat[s[i-1],s[i]]:=1;
              mat[s[i],s[i-1]]:=1;
             end;
    end;
  end;
end;
begin
 clrscr;
 writeln;
 write('Numarul de varfuri : ');readln(n);
 write('Numarul de muchii : ');readln(m);
 for i:=1 to n do
  for j:=1 to n do
   mat[i,j]:=0;
 for i:=1 to m do
  begin
   write('Muchia ',i,' : ');
   readln(j,k);
   mat[j,k]:=1;
   mat[k,j]:=1;
  end;
 nr_sol:=0;
 asfalt(1);
 if nr_sol=0 then writeln('Nu exista solutii.');
 readkey;
end.