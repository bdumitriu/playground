Program OJ_96;
uses crt;
type sir=array[1..4] of shortint;
     matrice=array[1..5,1..5] of 0..9;
const x:sir=(0,1,0,-1);
      y:sir=(1,0,-1,0);
var mat,s:matrice;
    i,j,m,n,cul,max,d:integer;

Function gata:boolean;
var f,g:integer;
begin
 gata:=true;
 for f:=1 to m do
  for g:=1 to n do
   if s[f,g]<>0 then gata:=false;
end;

Procedure culoare(i,j,cul:integer);
var k:byte;
    ii,jj:shortint;
begin
 for k:=1 to 4 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
   if (ii in [1..m]) and (jj in [1..n]) then
    if s[ii,jj]=cul then
                     begin
                      d:=d+1;
                      s[ii,jj]:=0;
                      culoare(ii,jj,cul);
                     end;
  end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce o matrice ');
 writeln('cu elemente intre 1 si 9 reprezentand culori va va afisa pt.');
 writeln('fiecare culoare numarul maxim de elemente a unei multimi conexe');
 writeln('a respectivei culori unde multime conexa a unei culori = toate');
 writeln('elementele la care se poate ajunge din elementul considerat, mer-');
 writeln('gand pe liniile si coloanele matricii, pastrand culoarea.');
 writeln;
 textcolor(lightblue);
 write('Introduceti numarul de linii ale matricii : ');readln(m);
 write('Introduceti numarul de coloane a matricii : ');readln(n);
 for i:=1 to m do
  for j:=1 to n do
   begin
    write('Elementul ',i,',',j,' : ');readln(mat[i,j]);
    s[i,j]:=mat[i,j];
   end;
 writeln;
 textcolor(yellow);
 cul:=1;
 repeat
  max:=0;
  for i:=1 to m do
   for j:=1 to n do
    begin
     if s[i,j]=cul then
                    begin
                     s[i,j]:=0;
                     d:=1;
                     culoare(i,j,cul);
                     if d>max then max:=d;
                    end;
    end;
  if max>0 then writeln('Numarul maxim de elemente pt. culoarea ',cul,' este : '
  ,max);
  cul:=cul+1;
 until gata;
 readkey;
end.