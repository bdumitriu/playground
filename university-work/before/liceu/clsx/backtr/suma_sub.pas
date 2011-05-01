Program generarea_submultimilor_unei_multimi_cu_propriteate;
uses crt;
var i,n,d,la:byte;
    s,m:array [1..100] of integer;

Procedure scrie(i:byte);
var x:byte;
begin
 for x:=1 to i do
  write(m[s[x]],' ');
 readln;
end;

Function suma(i:byte):boolean;
var x,k,y,z,q:byte;
    a:integer;
    max,p:array[1..100] of integer;
    ver:boolean;
begin
 q:=i;
 k:=0;
 for x:=1 to n+1 do
  p[x]:=0;
 for x:=1 to n do
  p[x]:=s[x];
 repeat
  k:=k+1;
  max[k]:=0;
  for x:=1 to i do
   if max[k]<m[p[x]] then
                      begin
                       max[k]:=m[p[x]];
                       y:=x;
                      end;
  for x:=y to i do
   p[x]:=p[x+1];
  i:=i-1;
 until k=d;
 a:=0;
 for x:=1 to d do
  a:=a+max[x];
 z:=0;
 for x:=1 to n do
  begin
   ver:=true;
   for y:=1 to q do
    if m[x] = m[s[y]] then ver:=false;
   if ver then z:=z+m[x];
  end;
 if a<z then suma:=true
        else suma:=false;
 i:=q;
end;

Function ok(i:byte):boolean;
var x:byte;
begin
 ok:=true;
 for x:=1 to i-1 do
  if s[x]>=s[i] then ok:=false;
end;

Procedure subm(i:byte);
var j:byte;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if ok(i) then
   for la:=d to n do
    if i<la then subm(i+1)
            else {if  then} if suma(i) then scrie(i);
  end;
end;

begin
 clrscr;
 writeln;
 writeln('Acesta este un program care daca ii veti introduce o multime M ');
 writeln('va verifica daca exista o submultime a multimii M cu proprietatea');
 writeln('ca suma a oricaror m elemente din submultime este mai mica decat ');
 writeln('suma celorlalte elementelor ramase in multimea M, iar in caz ca a-.');
 writeln('ceasta submultime exista o va afisa.');
 writeln;
 write('Numarul de elemente al multimii este : ');readln(n);
 for i:=1 to n do
  begin
   write('Elementul ',i,' este : ');readln(m[i]);
  end;
 writeln('Acum introduceti m (numarul de elemente din submultime ce trebuie');
 writeln('insumate). ATENTIE : m trebuie sa fie < decat n !!! ');
 repeat
  write('m=');readln(d);
 until (d<n) and (d>0);
 writeln;
 subm(1);
 readkey;
end.