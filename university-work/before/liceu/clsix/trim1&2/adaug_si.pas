Program adaugare_in_sir;
uses crt;
var n,m,i,p:integer;
    a:array[1..50] of integer;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce un sir de nu-');
 writeln('mere si un alt 2 numere m si p va va adauga numarul p in sir pe');
 writeln('pozitia m.');
 writeln;
 textcolor(lightblue);
 write('Introduceti numarul de elemente ale sirului : ');readln(n);
 for i:=1 to n do
  begin
   write('Elementul ',i,' : ');readln(a[i]);
  end;
 write('m=');readln(m);
 write('p=');readln(p);
 writeln;
 textcolor(yellow);
 for i:=n downto m do
  a[i+1]:=a[i];
 a[m]:=p;
 for i:=1 to n+1 do
  write(a[i],' ');
 readkey;
end.