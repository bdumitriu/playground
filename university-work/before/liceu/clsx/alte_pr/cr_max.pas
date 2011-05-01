Program subsir_crescator_de_lungime_maxima;
uses crt;
var c,d,i,j,n,m,gata,k:integer;
    a,l:array[1..100] of integer;

Function max(i:byte):integer;
var j,maxx:integer;
begin
 maxx:=0;
 for j:=i+1 to n do
  if a[j]>a[i] then if l[j]>maxx then maxx:=l[j];
 max:=maxx;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce un sir ne-');
 writeln('ordonat de numare va va afisa subsirul crescator de lungime ');
 writeln('maxima al sirului initial.');
 writeln;
 textcolor(lightblue);
 write('Introduceti numarul de elemente ale sirului : ');readln(n);
 for i:=1 to n do
  begin
   write('Elementul ',i,' : ');
   readln(a[i]);
  end;
 writeln;
 textcolor(yellow);
 for i:=1 to n do
  l[i]:=1;
 for i:=n downto 1 do
  l[i]:=l[i]+max(i);
 write('Subsirul maxim crescator : ');
 m:=0;
 for i:=1 to n do
  if l[i]>m then
             begin
              m:=l[i];
              c:=i;
             end;
 write(a[c],' ');
 gata:=l[c];
 k:=1;d:=c;
 repeat
  m:=0;
  for i:=c+1 to n do
   if a[i]>=a[d] then if l[i]>m then
                                 begin
                                 m:=l[i];
                                 c:=i;
                                end;

  write(a[c],' ');
  k:=k+1;
 until k=gata;
 readkey;
end.