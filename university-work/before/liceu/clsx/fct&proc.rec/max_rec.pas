Program maxim_din_un_sir_recursiv;
uses crt;
var n,i,max:integer;
    a:array[1..100] of integer;
    x:array[1..100] of byte;

Function maxim(m:integer):integer;
var j:integer;
begin
 for j:=m-1 downto 1 do
  begin
   if a[m]>=a[j] then if x[m]=0 then maxim:=a[m];
   if a[m]< a[j] then if x[m]=0 then
                                   begin
                                    x[m]:=1;
                                    maxim:=maxim(j);
                                   end;
  end;
end;

begin
 clrscr;
 writeln;
 writeln('Acesta este un program care daca ii veti introduce un sir de numere');
 writeln('va va afisa numarul maxim din acel sir.');
 writeln;
 write('Numarul de elemente din sir este : ');readln(n);
 for i:=1 to n do
  begin
   write('Elementul ',i,' este ');
   readln(a[i]);
  end;
 for i:=1 to 100 do
  x[i]:=0;
 max:=maxim(n);
 writeln('Elementul maxim din sirul introdus de dumneavoastra este ',max,'.');
 readkey;
end.