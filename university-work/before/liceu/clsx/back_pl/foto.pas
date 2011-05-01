Program obiectul_din_fotografie;
uses crt;
type sir=array[1..8] of shortint;
const y:sir=(0,1,1,1,0,-1,-1,-1);
      x:sir=(-1,-1,0,1,1,1,0,-1);
var mat,s:array[1..20,1..20] of byte;
    i,j,l,nr,n:byte;
    o_bucata:boolean;

Procedure verif(i,j:shortint);
var k:byte;
    ii,jj:shortint;
begin
 for k:=1 to 8 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
    if (ii in [1..n]) and (jj in [1..n]) then
     if (mat[ii,jj]=1) and (s[ii,jj]=0) then
      begin
       s[ii,jj]:=1;
       verif(ii,jj);
      end;
  end;
end;

begin
 clrscr;
 writeln;
 write('Dimensiunea matricii ce simbolizeaza fotografia : ');readln(n);
 write('Numarul de elemente al matricii cu valoarea 1 : ');readln(nr);
 for i:=1 to n do
  for j:=1 to n do
   begin
    s[i,j]:=0;
    mat[i,j]:=0;
   end;
 for l:=1 to nr do
  begin
   write('Elementul ',l,' : ');
   readln(i,j);
   mat[i,j]:=1;
  end;
 verif(i,j);
 o_bucata:=true;
 for i:=1 to n do
  for j:=1 to n do
   if mat[i,j]<>s[i,j] then o_bucata:=false;
 if o_bucata then writeln('Obiectul este dintr-o bucata.')
             else writeln('Obiectul nu este dintr-o bucata.');
 readkey;
end.