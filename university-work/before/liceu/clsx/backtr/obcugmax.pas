Program n_obiecte_de_greutati_si_valori_diferite;
uses crt;
type obiect=record
             gr,val:integer;
            end;
var ob:array [1..100] of obiect;
    s,sol:array [1..100] of integer;
    n,i,g,max,contor_max,greutate,valoare:integer;

{Procedure scrie(i:byte);
var x:byte;
begin
 for x:=1 to i do
  write(s[x]:2);
 readln;
end;}

Function gr_tot(i:byte):integer;
var x:byte;
    aux:integer;
begin
 aux:=0;
 for x:=1 to i do
  with ob[s[x]] do
   aux:=aux+gr;
 gr_tot:=aux;
end;

Function val_tot(i:byte):integer;
var x:byte;
    aux:integer;
begin
 aux:=0;
 for x:=1 to i do
  with ob[s[x]] do
   aux:=aux+val;
 val_tot:=aux;
end;

Procedure atrib(i:byte);
var x:byte;
begin
 for x:=1 to i do
  sol[x]:=s[x];
 contor_max:=i;
 greutate:=gr_tot(i);
 max:=val_tot(i);
 valoare:=max;
end;

Function ok(i:byte):boolean;
var x:byte;
begin
 ok:=true;
 for x:=1 to i-1 do
  if s[x]>=s[i] then ok:=false;
end;

Procedure aranjare(i:byte);
var j:byte;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if (ok(i)) and (gr_tot(i)<=g) then aranjare(i+1)
                                 else if max < val_tot(i-1) then atrib(i-1);
  end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce greutatea si ');
 writeln('valoarea a n obiecte si apoi o greutate oarecare va va determina ');
 writeln('multimea de obiecte de valoare totala maxima si greutate totala ');
 writeln('ce nu depaseste greutatea introdusa de dumneavoastra.');
 writeln;
 textcolor(lightblue);
 write('Numarul de obiecte pe care doriti sa le introduceti : ');readln(n);
 for i:=1 to n do
  with ob[i] do
   begin
    write('Valoarea obiectului ',i,' : ');readln(val);
    write('Greutatea obiectului ',i,' : ');readln(gr);
   end;
 write('Greutatea totala ce poate fi atinsa : ');readln(g);
 writeln;
 textcolor(yellow);
 max:=0;
 aranjare(1);
 if sol[1]<>0 then
               begin
                write('Multimea de obiecte este : ');
                for i:=1 to contor_max do
                 write(sol[i]:3);
                writeln;
                writeln('Greutatea totala a obiectelor din multime este ',greutate,'.');
                writeln('Valoarea totala a obiectelor din multime este ',valoare,'.');
               end;
 readkey;
end.