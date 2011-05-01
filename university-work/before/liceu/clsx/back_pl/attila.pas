Program Attila_si_regele;
uses crt;
type sir=array[1..8] of integer;
const x:sir=(-2,-1,1,2,2,1,-1,-2);
      y:sir=(1,2,2,1,-1,-2,-2,-1);
var mat,s:array[1..15,1..15] of integer;
    n,i,j,l,nr,ci,cj,ri,rj,nr_sol:integer;

Procedure scrie;
var a,b:byte;
begin
 nr_sol:=nr_sol+1;
 writeln('Solutia ',nr_sol,' : ');
 for a:=1 to n do
  begin
   for b:=1 to n do
    write(mat[a,b]:3);
   writeln;
  end;
 readln;
end;

Procedure rege(i,j,pas:byte);
var k:byte;
    ii,jj:shortint;
begin
 for k:=1 to 8 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
   if (ii in [1..n]) and (jj in [1..n]) then
    if s[ii,jj]=0 then
                   begin
                    mat[ii,jj]:=pas;
                    if (ii=ci) and (jj=cj) then
                                            begin
                                             mat[ci,cj]:=1;
                                             scrie;
                                            end
                                           else
                                            begin
                                             s[ii,jj]:=1;
                                             rege(ii,jj,pas+1);
                                            end;
                    s[ii,jj]:=0;
                    mat[ii,jj]:=0;
                   end;
  end;
end;

Procedure cal(i,j,pas:byte);
var k:byte;
    ii,jj:shortint;
begin
 for k:=1 to 8 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
   if (ii in [1..n]) and (jj in [1..n]) then
    if s[ii,jj]=0 then
                   begin
                    mat[ii,jj]:=pas;
                    if (ii=ri) and (jj=rj) then
                                            begin
                                             s[ii,jj]:=1;
                                             rege(ri,rj,pas+1);
                                            end
                                           else
                                            begin
                                             s[ii,jj]:=1;
                                             cal(ii,jj,pas+1);
                                            end;
                    s[ii,jj]:=0;
                    mat[ii,jj]:=0;
                   end;
  end;
end;

begin
 clrscr;
 writeln;
 write('Dimensiunea tablei : ');readln(n);
 write('Pozitia initiala a calului : ');readln(ci,cj);
 write('Pozitia initiala a regelui : ');readln(ri,rj);
 write('Numarul de campuri arse initial : ');readln(nr);
 for i:=1 to n do
  for j:=1 to n do
   begin
    mat[i,j]:=0;
    s[i,j]:=0;
   end;
 for l:=1 to nr do
  begin
   write('Campul ars ',l,' : ');
   readln(i,j);
   s[i,j]:=1;
  end;
 mat[ci,cj]:=1;
 nr_sol:=0;
 cal(ci,cj,2);
 if nr_sol=0 then writeln('Nu exista solutii.');
 readkey;
end.