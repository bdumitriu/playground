Program grupuri_de_persoane;
uses crt;
var n,nr:integer;
    i,j:byte;
    c:array[1..50,1..50] of 0..1;
    s,pus:array[1..50] of integer;

Procedure scrie;
var x,y,max:integer;
begin
 nr:=nr+1;
 writeln('Solutia ',nr,' : ');
 max:=0;
 for x:=1 to n do
  if max<s[x] then max:=s[x];
 for x:=1 to max do
  begin
   write('Grupul ',x,' : ');
   for y:=1 to n do
    if s[y]=x then write(y,' ');
   writeln;
  end;
 readln;
end;

Function ok(i:byte):boolean;
var x:byte;
    aux:boolean;
begin
 ok:=true;
 aux:=true;
 if pus[i]=1 then begin
                   ok:=false;
                   aux:=false;
                  end;
 for x:=1 to i-1 do
  if s[i]=s[x] then
   if c[i,x]=0 then
                begin
                 ok:=false;
                 aux:=false;
                end;
end;

Procedure grup(i:byte);
var k,j,max:byte;
begin
 max:=0;
 for k:=1 to i-1 do
  if s[k]>max then max:=s[k];
 for j:=1 to max+1 do
  begin
   s[i]:=j;
   if ok(i) then
    begin
     pus[i]:=1;
     if i=n then scrie
            else grup(i+1);
     pus[i]:=0;
    end;
  end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de persoane : ');readln(n);
 for i:=1 to n do
  pus[i]:=0;
 for i:=1 to n do
  for j:=1 to n do
   if i=j then c[i,j]:=1;
 for i:=1 to n do
  for j:=1 to n do
   if i<>j then
    begin
     write('Relatia intre ',i,' si ',j,' : ');
     readln(c[i,j]);
    end;
 nr:=0;
 writeln;
 grup(1);
 if nr=0 then writeln('Nu exista solutii.');
 readkey;
end.