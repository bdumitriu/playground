Program numere_cu_n_cifre_a_caror_suma_e_s;
uses crt;
var a:array[1..100] of 0..9;
    n,s,nr,nr_tot:integer;

Procedure scrie;
var x:integer;
begin
 nr:=nr+1;
 for x:=1 to n do write(a[x]);
 readln;
 if nr_tot=nr then halt;
end;

Function suma(i:byte):integer;
var x:byte;
    y:integer;
begin
 y:=0;
 for x:=1 to i do
  y:=y+a[x];
 suma:=y;
end;

Function ok(i:byte):boolean;
begin
 ok:=true;
 if (i=1) and (a[i]=0) then ok:=false;
end;

Procedure numere(i:byte);
var j:byte;
begin
 for j:=0 to 9 do
  begin
   a[i]:=j;
   if ok(i) then
    begin
     if (suma(i)=s) and (i=n) then scrie;
     if (suma(i)<s) and (i<n) then numere(i+1);
    end;
  end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce doua numere :');
 writeln('n si s va va afisa toate numerele de n cifre, cu suma cifrelor');
 writeln('este s.');
 writeln;
 textcolor(lightblue);
 write('n=');readln(n);
 write('s=');readln(s);
 write('Introduceti numarul de solutii care doriti sa fie afisate : ');
 readln(nr_tot);
 writeln;
 textcolor(yellow);
 nr:=0;
 numere(1);
 if nr=0 then writeln('Nu exista solutii.');
 readkey;
end.