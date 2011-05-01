Program parcurgerea_unui_graf_in_latime;
uses crt;
var i,j,k,n,m,p,u,v,x:integer;
    a:array[1..100,1..100] of 0..1;
    viz:array[1..100] of 0..1;
    c:array[1..100] of 0..100;

begin
 clrscr;
 writeln;
 writeln('Acesta este un program care va parcurge un graf dat de dvs.');
 write('Numarul de varfuri este : ');readln(n);
 write('Numarul de muchii este : ');readln(m);
 for i:=1 to n do
  for j:=1 to n do
   a[i,j]:=0;
 for k:=1 to m do
  begin
   write('Muchia ',k,' este : ');readln(i,j);
   a[i,j]:=1;a[j,i]:=1;
  end;
 write('Varful initial este : ');readln(v);
 for i:=1 to n do
  viz[i]:=0;
 writeln('Asa va fi vizitat graful in ordinea de parcurgere BF (BreadthFirst) : ');
 write(v,' ');
 p:=1;
 u:=1;
 c[1]:=v;
 viz[v]:=1;
 while p<=u do
  begin
   x:=c[p];
   for i:=1 to n do
    if (a[x,i]=1) and (viz[i]=0) then
                                  begin
                                   write(i,' ');
                                   viz[i]:=1;
                                   u:=u+1;
                                   c[u]:=i;
                                  end;
   p:=p+1;
  end;
 readkey;
end.