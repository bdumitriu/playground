Program determinare_cicluri_euleriene;
uses crt;
label 1;
var n,i,j,a,nr,nr_m:integer;
    mat:array[1..50,1..50] of integer;
    s:array[1..50] of integer;

Procedure scrie(i:byte);
var x:byte;
begin
 nr:=nr+1;
 write('Ciclul ',nr,' : ');
 for x:=1 to i do
  write(s[i],' ');
end;

Function ok(i:byte):boolean;
var x:byte;
begin
 ok:=true;
 if mat[s[i],s[i-1]]=0 then ok:=false;
 if s[i]=s[i-1] then ok:=false;
 for x:=1 to i-2 do
  if s[x]=s[i] then
                begin
                 if s[x-1]=s[i-1] then ok:=false;
                 if s[x+1]=s[i-1] then ok:=false;
                end;
end;

Procedure euler(i:byte);
var j:byte;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if ok(i) then
    if i-1=nr_m then scrie(i)
                else euler(i+1);
  end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care determina toate ciclurile euleriene');
 writeln('ale unui graf in caz ca acesta este eulerian.');
 textcolor(lightblue);
 writeln;
 write('Numarul de varfuri ale grafului : ');
 readln(n);
 for i:=1 to n do
  for j:=1 to n do
   begin
    write('Matrice de adiacenta [',i,',',j,'] : ');
    readln(mat[i,j]);
    if mat[i,j]=1 then nr_m:=nr_m+1;
   end;
 nr:=0;
 nr_m:=nr_m div 2;
 textcolor(yellow);
 writeln;
 a:=0;
 for i:=1 to n do
  begin
   for j:=1 to n do
    if mat[i,j]=1 then
                   begin
                    a:=a+1;
                    if a>=2 then s[1]:=i;
                   end;
   if a div 2 = 1 then
                   begin
                    writeln('Graful nu are cicluri euleriene.');
                    goto 1;
                   end;
   a:=0;
  end;
 euler(2);
 if nr = 0 then writeln('Graful nu are cicluri euleriene.');
 1:textcolor(lightgray);
 readkey;
end.