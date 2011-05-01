Program iesire_din_labirint;
uses crt;
type sir = array [1..4] of shortint;
const x : sir = (-1, 0, 1, 0);
      y : sir = (0, 1, 0, -1);
var s, mat : array [1..25, 1..25] of integer;
    i, j, l, m, n, ii, jj, nr_sol : byte;
    nr : word;

Procedure scrie;
var u,v:byte;
begin
 nr_sol:=nr_sol+1;
 writeln('Solutia ',nr_sol,' : ');
 for u:=1 to m do
  begin
   for v:=1 to n do
    write(s[u,v]:3);
   writeln;
  end;
 readln;
end;

Function ok(iii,jjj:shortint):boolean;
begin
 ok:=false;
 if (iii in [1..m]) and (jjj in [1..n]) then ok:=true;
end;

Procedure iesire(ii,jj,pas:byte);
var iii,jjj:shortint;
    k:byte;
begin
 for k:=1 to 4 do
  begin
   iii:=ii+x[k];
   jjj:=jj+y[k];
   if ok(iii,jjj) then
    if (mat[iii,jjj]=1) and (s[iii,jjj]=0) then
     begin
      s[iii,jjj]:=pas;
      if (iii in [1,m]) or (jjj in [1,n]) then scrie
                                          else iesire(iii,jjj,pas+1);
      s[iii,jjj]:=0;
     end;
  end;
end;

begin
 clrscr;
 writeln('Introduceti cofiguratia unui labirint sub forma de matrice in');
 writeln('felul urmator : daca o camera e accesibila atunci ea va fi co-');
 writeln('dificata cu 1.');
 writeln;
 repeat
  write('Numarul de linii a matricii (maxim 25) : ');
  readln(m);
 until (m>0) and (m<=25);
 repeat
  write('Numarul de coloane a matricii (maxim 25) : ');
  readln(n);
 until (n>0) and (n<=25);
 for i:=1 to m do
  for j:=1 to n do
   begin
    mat[i,j]:=0;
    s[i,j]:=0;
   end;
 writeln;
 write('Dati numarul de camere accesibile : ');read(nr);
 for l:=1 to nr do
  begin
   write('Coordonatele camerei accesibile ',l,' : ');
   readln(i,j);
   mat[i,j]:=1;
  end;
 writeln;
 write('Coordonatele punctului de plecare : ');readln(ii,jj);
 nr_sol:=0;
 s[ii,jj]:=1;
 clrscr;
 iesire(ii,jj,2);
 readkey;
end.