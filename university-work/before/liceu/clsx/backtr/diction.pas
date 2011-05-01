Program traduceri_cu_n_dictionare;
uses crt;
type dic=record
	  li,lf:1..50;
	 end;
var n,i,j,m,nr_sol,a,b:integer;
    s:array[1..50] of integer;
    ttt:array[1..50] of dic;

Procedure scrie(k:integer);
var x:integer;
begin
 nr_sol:=nr_sol+1;
 writeln('Solutia ',nr_sol);
 for x:=1 to k do
  write(s[x],' ');
 readln;
end;

Function ok(k:integer):boolean;
var x:integer;
begin
 ok:=true;
 if k=1 then if ttt[s[1]].li<>i then ok:=false;
 if k>1 then if ttt[s[k]].li<>ttt[s[k-1]].lf then ok:=false;
 for x:=1 to k-1 do
  if ttt[s[k]].lf=ttt[s[x]].li then ok:=false;
end;

Procedure dict(k:byte);
var l:byte;
begin
 for l:=1 to n do
  begin
   s[k]:=l;
   if ok(k) then
    if ttt[s[k]].lf=j then scrie(k)
		      else dict(k+1);
  end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de limbi : ');readln(m);
 write('Numarul de dictionare : ');readln(n);
 for i:=1 to n do
  begin
   write('Dictionarul ',i,' : ');readln(a,b);
   ttt[i].li:=a;
   ttt[i].lf:=b;
  end;
 write('Limba din care trebuie tradus : ');readln(i);
 write('Limba in care trebuie tradus : ');readln(j);
 nr_sol:=0;
 dict(1);
 if nr_sol=0 then writeln('Nu exista solutii.');
 readkey;
end.