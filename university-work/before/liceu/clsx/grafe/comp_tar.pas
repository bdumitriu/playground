Program de_determinare_a_componentelor_tare_conexe_a_unui_graf;
uses crt;
var l:set of byte;
    comp:array[1..10] of set of byte;
    i,j,k,nc,m,n,x,y:integer;
    s,p:set of byte;
    a:array[1..30,1..30] of byte;

Procedure det_s(i:byte);
begin
 s:=[];
 for j:=1 to n do
  if a[i,j]=1 then s:=s+[j];
end;

Procedure det_p(i:byte);
begin
 p:=[];
 for j:=1 to n do
  if a[j,i]=1 then p:=p+[j];
end;

begin
 clrscr;
 writeln;
 write(' Introduceti numarul de varfuri ale grafului : ');
 readln(n);
 write(' Introduceti numarul de muchii ale grafului : ');
 readln(m);
 for i:=1 to n do
  for j:=1 to n do
   a[i,j]:=0;
 for i:=1 to m do
  begin
   write(' Arcul ',i,' : ');readln(x,y);
   a[x,y]:=1;
  end;
 for k:=1 to n do
  for i:=1 to n do
   for j:=1 to n do
    if a[i,j]=0 then a[i,j]:=a[i,k]*a[k,j];
 l:=[];
 for i:=1 to 10 do
  comp[i]:=[];
 nc:=0;
 for i:=1 to n do
  if not (i in l) then
                   begin
                    inc(nc);
                    det_s(i);
                    det_p(i);
                    comp[nc]:=s*p+[i];
                    l:=l+comp[nc];
                   end;
 for i:=1 to nc do
  begin
   write(' Componenta conexa ',i,' : ');
   for j:=1 to n do
    if j in comp[i] then write(j,' ');
   writeln;
  end;
 readkey;
end.