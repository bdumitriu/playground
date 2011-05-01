Program delegatie_de_femei;
uses crt;
var f,k,l,n,b,m:integer;
    s:array[1..50] of integer;
    nr_sol:integer;

Procedure scrie;
var x:integer;
begin
 nr_sol:=nr_sol+1;
 writeln('Solutia ',nr_sol);
 for x:=1 to l do
  write('f',s[x],' ');
 for x:=l+1 to k do
  write('b',s[x],' ');
 readln;
end;

Procedure barbati(i:byte);
var j,z:integer;
    aux:boolean;
begin
 for j:=1 to b do
  begin
   s[i]:=j;
   aux:=true;
   for z:=l+1 to i-1 do if s[z]>=s[i] then aux:=false;
   if aux then
    if i=k then scrie
	   else barbati(i+1);
  end;
end;

Procedure femei(i:byte);
var j,z:integer;
    aux:boolean;
begin
 for j:=1 to f do
  begin
   s[i]:=j;
   aux:=true;
   for z:=1 to i-1 do if s[z]>=s[i] then aux:=false;
   if aux then
    if i=l then barbati(i+1)
	   else femei(i+1);

  end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de persoane : ');readln(n);
 write('Numarul de femei : ');readln(f);
 write('Numarul de persoane din delegatie : ');readln(k);
 write('Numarul de femei din delegatie : ');readln(l);
 b:=n-f;
 m:=k-l;
 nr_sol:=0;
 femei(1);
 if nr_sol=0 then writeln('Nu exista solutii.');
 readkey;
end.