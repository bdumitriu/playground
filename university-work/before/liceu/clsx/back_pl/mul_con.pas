Program matrice_cu_multimi_conexe;
uses crt;
type sir=array[1..4] of shortint;
     matrice=array[1..5,1..5] of integer;
const x:sir=(0,1,0,-1);
      y:sir=(1,0,-1,0);
var mat,s:array[1..5,1..5] of 0..9;
    m,n,i,j,max,cl,cc,cul,c,d:integer;

Function gata:boolean;
var f,g:integer;
begin
 gata:=true;
 for f:=1 to m do
  for g:=1 to n do
   if s[f,g]<>0 then gata:=false;
end;

Procedure con(i,j,c:byte);
var ii,jj:shortint;
    k:byte;
begin
 for k:=1 to 4 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
   if (ii in [1..m]) and (jj in [1..n]) then
    if s[ii,jj]=c then
                   begin
                    s[ii,jj]:=0;
                    d:=d+1;
                    con(ii,jj,c);
                   end;
  end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce o matrice cu');
 writeln('m linii si n coloane, avand ca elemente cifre de la 1 la 9, repre-');
 writeln('zentand culori va determina numarul maxim de elemente ale unei ');
 writeln('multimi conexe si culoarea acestora. Se defineste ca multime co-');
 writeln('nexa a unui element ca fiind formata din acele elemente in care se');
 writeln('poate ajunge prin din elementul considerat prin deplasari succe-');
 writeln('sive, pe linii si coloane.');
 textcolor(lightblue);
 writeln;
 write('Numarul de linii ale matricii : ');readln(m);
 write('Numarul de coloane ale matricii : ');readln(n);
 for i:=1 to m do
  for j:=1 to n do
   s[i,j]:=mat[i,j];
 for i:=1 to m do
  for j:=1 to n do
   begin
    write('Elementul [',i,',',j,'] : ');
    readln(mat[i,j]);
    s[i,j]:=mat[i,j];
   end;
 max:=0;
 repeat
  for i:=1 to m do
   for j:=1 to n do
    if s[i,j]<>0 then
                    begin
                     s[i,j]:=0;
                     cl:=i;
                     cc:=j;
                     i:=m;
                     j:=n;
                     c:=mat[cl,cc]
                    end;
  d:=1;
  con(cl,cc,c);
  if max<d then
            begin
             cul:=c;
             max:=d;
            end;
 until gata;
 textcolor(yellow);
 writeln;
 writeln('Numarul maxim de elemente dintr-o multime conexa este ',max);
 writeln('Culoarea multimii cu nr. maxim de elemente este culoarea cod ',cul);
 readkey;
end.