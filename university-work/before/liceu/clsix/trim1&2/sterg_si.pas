Program de_stergere_din_sir;
uses crt;
var n,m,i:integer;
    a:array[1..50] of integer;
begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce un sir de ');
 writeln('numere si apoi un numar m va va sterge numarul de pe pozitia');
 writeln('m din sir.');
 writeln;
 textcolor(lightblue);
 write('Numarul de elmente din sir : ');readln(n);
 for i:=1 to n do
  begin
   write('Elementul ',i,' : ');
   readln(a[i]);
  end;
 write('m=');readln(m);
 writeln;
 textcolor(yellow);
 for i:=m to n do
  a[i]:=a[i+1];
 for i:=1 to n-1 do
  write(a[i],' ');
 readkey;
end.