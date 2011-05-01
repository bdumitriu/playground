Program trasee_cal_pe_tabla_de_sah;
uses crt;
type sir=array [1..8] of shortint;
const x:sir=(-2,-1,1,2,2,1,-1,-2);
      y:sir=(1,2,2,1,-1,-2,-2,-1);
var t:array[1..15,1..15] of byte;
    nn,n,i,j:byte;
    nr_sol:word;

Procedure Scrie;
var a,b:byte;
begin
 nr_sol:=nr_sol+1;
 writeln('Solutia ',nr_sol,' :');
 for a:=1 to n do
  begin
   for b:=1 to n do
    write(t[a,b]:4);
   writeln;
  end;
 readln;
end;

Function Ok(ii,jj:shortint):boolean;
begin
 ok:=false;
 if (ii >= 1) and (ii <= n) and (jj >= 1) and (jj <= n) then ok:=true;
end;

Procedure Mutare(i,j,pas:byte);
var ii,jj:shortint;
    k:byte;
begin
 for k:=1 to 8 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
   if ok(ii,jj) then
    if t[ii,jj]=0 then
     begin
      t[ii,jj]:=pas;
      if pas=nn then Scrie
                else Mutare(ii,jj,pas+1);
      t[ii,jj]:=0;
     end;
  end;
end;

begin
 clrscr;
 writeln;
 writeln('Acesta este un program care daca ii veti introduce dimensiunile');
 writeln('unei table de sah (tabla este patratica n * n patratele) va ');
 writeln('afisa toate posibilitatile de a parcurge tabla de sah cu un cal ');
 writeln('plecand din coltul din stanga sus si trecand prin toate patrate-');
 writeln('lele tablei. ATENTIE : DACA DIMENSIUNILE TABLEI DE SAH SUNT PREA ');
 writeln('MARI VA TREBUI SA ASTEPTATI MAI MULT PANA LA AFISAREA SOLUTIILOR!');
 repeat
  write('Dimensiunea tablei este (maxim 15) : ');
  readln(n);
 until (n>0) and (n<=15);
 for i:=1 to n do
  for j:=1 to n do t[i,j]:=0;
 t[1,1]:=1;
 nn:=n*n;
 nr_sol:=0;
 writeln;
 Mutare(1,1,2);
 if nr_sol=0 then writeln('Problema nu are solutie.');
 readkey;
end.