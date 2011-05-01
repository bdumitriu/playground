Program OJ_91;
uses crt;
type sir=array[1..8] of shortint;
     matrice=array[1..15,1..15] of 0..1;
const x:sir=(-2,-1,1,2,2,1,-1,-2);
      y:sir=(1,2,2,1,-1,-2,-2,-1);
var mat,s:matrice;
    m,n,i,j:byte;
    nr_sol:word;

Procedure scrie;
var i,j:byte;
begin
 nr_sol:=nr_sol+1;
 writeln('Solutia ',nr_sol,' :');
 for i:=1 to m do
  begin
   for j:=1 to n do
    write(s[i,j]:3);
   writeln;
  end;
 readln;
end;

Procedure intors(i,j,pas:byte);
var ii,jj:shortint;
    k:byte;
begin
 for k:=1 to 8 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
   if (ii in [1..m]) and (jj in [1..n]) then
    if mat[ii,jj]=0 then
     begin
      mat[ii,jj]:=1;
      s[ii,jj]:=pas;
      if (ii=1) and (jj=1) then
                            begin
                             s[1,1]:=1;
                             scrie;
                            end
                           else intors(ii,jj,pas+1);
      mat[ii,jj]:=0;
      s[ii,jj]:=0;
     end;
   end;
end;

Procedure dus(i,j,pas:byte);
var ii,jj:shortint;
    k:byte;
begin
 for k:=1 to 8 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
   if (ii in [1..m]) and (jj in [1..n]) then
    if mat[ii,jj]=0 then
     begin
      mat[ii,jj]:=1;
      s[ii,jj]:=pas;
      if (ii=m) and (jj=n) then intors(ii,jj,pas+1)
                           else dus(ii,jj,pas+1);
      mat[ii,jj]:=0;
      s[ii,jj]:=0;
     end;
   end;
end;

begin
 clrscr;
 writeln;
 textcolor(10);
 writeln('Acesta este un program care daca ii veti introduce un lac inghe-');
 writeln('tat sub forma unei matrici cu mat[i,j]=0 daca pozitia i,j este');
 writeln('gheata si mat[i,j]=1 daca pe pozitia i,j este gaura in gheata va');
 writeln('afisa toate solutiile pentru o broscuta ce se gaseste in poziti-');
 writeln('1,1 sa ajunga la o insecta in pozitia m,n si sa vina inapoi sti-');
 writeln('ind ca broscuta sare precum calul de sah, nu poate sari decat pe');
 writeln('gheata, iar in momentul in care paraseste bucata de gheata aceasta');
 writeln('se sparge (cu exceptia primei).');
 writeln;
 textcolor(9);
 write('Numarul de linii ale matricii ce reprezinta lacul : ');
 readln(m);
 write('Numarul de coloane ale matricii ce reprezinta lacul : ');
 readln(n);
 for i:=1 to m do
  for j:=1 to n do
   begin
    write('Elementul ',i,',',j,' : ');
    readln(mat[i,j]);
   end;
 textcolor(14);
 writeln;
 s[1,1]:=1;
 nr_sol:=0;
 dus(1,1,2);
 if nr_sol=0 then writeln('Nu exista solutii.');
 writeln(nr_sol);
 readkey;
end.
