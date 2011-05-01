Program parcurgerea_unui_graf_in_adancime;
uses crt;
var i,j,k,n,m,v,x,ps:integer;
    a:array[1..100,1..100] of 0..1;
    viz:array[1..100] of 0..1;
    s:array[1..100] of 0..100;

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
 writeln('Asa va fi vizitat graful in ordinea de parcurgere DF (DepthFirst) : ');
 write(v,' ');
 viz[v]:=1;
 ps:=1;
 s[ps]:=v;
 while ps>=1 do
  begin
   x:=s[ps];
   k:=1;
   while (k<=n) and ((a[x,k]=0) or ((a[x,k]=1) and (viz[k]=1))) do
                                                                 k:=k+1;
   if k=n+1 then
             ps:=ps-1
            else
             begin
              ps:=ps+1;
              s[ps]:=k;
              write(k,' ');
              viz[k]:=1;
             end;
  end;
 readkey;
end.