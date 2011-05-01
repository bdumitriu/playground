Program de_aranjare_a_unui_sir_in_ordine_crescatoare_QuickSort;
uses crt;
type vector=array[1..100] of integer;
var n,a,k:byte;
    x:vector;

Procedure poz(s,d:integer;var k:byte);
var y,aux:integer;
begin
 y:=x[(s+d) div 2];
 repeat
  while x[s]<y do s:=s+1;
  while x[d]>y do d:=d-1;
  if s<=d then
           begin
            aux:=x[s];
            x[s]:=x[d];
            x[d]:=aux;
           end;
 until s=d;
 k:=s;
end;

Procedure quick(s,d:integer);
begin
 if s<d then
  begin
   poz(s,d,k);
   quick(s,k-1);
   quick(k+1,d);
  end;
end;

begin
 clrscr;
 writeln;
 writeln('Acesta este un program care daca ii veti introduce un sir de nume-');
 writeln('re vi le va ordona pe acestea crescator prin metoda "Quick Sort"');
 writeln;
 write('Numarul de elemente din sir : ');readln(n);
 for a:=1 to n do
  begin
   write('Elementul ',a,' : ');
   readln(x[a]);
  end;
 quick(1,n);
 for a:=1 to n do
  write(x[a]:3);
 readkey;
end.