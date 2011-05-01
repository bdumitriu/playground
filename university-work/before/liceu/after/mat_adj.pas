Program de_determinare_a_matricii_adjuncte_a_unei_matrici;
uses crt;
type sir=array[1..7,1..7] of integer;
var a,b:array[1..8,1..8] of integer;
    c:sir;
    i,j,k,l,m,n,g:integer;

Function det(c:sir;g:integer):integer;
var y:integer;
begin
 if g=4 then
  begin
   y:=c[1,1]*c[2,2]*c[3,3]+c[3,1]*c[1,2]*c[2,3]+c[1,3]*c[2,1]*c[3,2];
   y:=y-c[1,3]*c[2,2]*c[3,1]-c[1,1]*c[2,3]*c[3,2]-c[1,2]*c[2,1]*c[3,3];
  end;
 if g=3 then y:=c[1,1]*c[2,2]-c[1,2]*c[2,1];
 det:=y;
end;

Function putere(p,q:integer):integer;
var x,y:integer;
begin
 y:=p;
 for x:=2 to q do
  y:=y*p;
 putere:=y;
end;

begin
 clrscr;
 writeln;
 write('Gradul matricii : (atentie : nu merge decat pt. 3 si 4) ');readln(g);
 for i:=1 to g do
  for j:=1 to g do
   begin
    write('Elementul ',i,',',j,' : ');
    readln(a[i,j]);
   end;
 for i:=1 to g do
  for j:=1 to  g do
   b[j,i]:=a[i,j];
 a:=b;
 for i:=1 to g do
  for j:=1 to g do
   b[i,j]:=0;
 for i:=1 to g do
  for j:=1 to g do
   begin
    b[i,j]:=putere(-1,i+j);
    m:=0;
    n:=0;
    for k:=1 to g do
     begin
      if k<>i then
               begin
                inc(m);
                n:=0;
               end;
      for l:=1 to g do
       if (k<>i) and (l<>j) then
                             begin
                              inc(n);
                              c[m,n]:=a[k,l];
                             end;
     end;
    b[i,j]:=b[i,j]*det(c,g);
   end;
 writeln('Matricea adjuncta este : ');
 for i:=1 to g do
  begin
   writeln;
   for j:=1 to g do
    write(b[i,j]:5);
  end;
 readkey;
end.