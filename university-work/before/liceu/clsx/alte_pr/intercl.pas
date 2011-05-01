Program interclasare_a_doua_siruri;
uses crt;
var a,b,c:array[1..1000] of integer;
    i,j,k,n,m:integer;
begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce doua siruri de');
 writeln('de numere ordonate crescator le va interclasa formand si afisand ');
 writeln('un al treilea sir ordonat. ');
 writeln;
 textcolor(lightblue);
 write('Introduceti numarul de elemente ale primului sir : ');readln(m);
 for i:=1 to m do
  begin
   write(' Elementul ',i,' : ');
   readln(a[i]);
  end;
 write('Introduceti numarul de elemente ale celui de-al doilea sir : ');
 readln(n);
 for j:=1 to n do
  begin
   write(' Elementul ',j,' : ');
   readln(b[j]);
  end;
 i:=1;
 j:=1;
 k:=0;
 repeat
  if a[i]<=b[j] then
                 begin
                  inc(k);
                  c[k]:=a[i];
                  if i<m+1 then inc(i);
                 end
                else
                 begin
                  inc(k);
                  c[k]:=b[j];
                  if j<n+1 then inc(j);
                 end;
 until (i=m+1) or (j=n+1);
 if i=m+1 then
         for i:=j to n do
          begin
           inc(k);
           c[k]:=b[i];
          end
        else
         for j:=i to m do
          begin
           inc(k);
           c[k]:=a[j];
          end;
 textcolor(yellow);
 writeln;
 writeln('Sirul obtinut : ');
 for k:=1 to m+n do
  write(c[k],' ');
 readkey;
end.