Program de_determinare_a_matricii_adjuncte_a_unei_matrici_de_ord_trei;
uses crt;
var a,b:array[1..3,1..3] of integer;
    c:array[1..2,1..2] of integer;
    i,j,k,l,m,n,g:integer;

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
 for i:=1 to 3 do
  for j:=1 to 3 do
   begin
    write('Elementul ',i,',',j,' : ');
    readln(a[i,j]);
   end;
 for i:=1 to 3 do
  for j:=1 to  3 do
   b[j,i]:=a[i,j];
 a:=b;
 for i:=1 to 3 do
  for j:=1 to 3 do
   b[i,j]:=0;
 for i:=1 to 3 do
  for j:=1 to 3 do
   begin
    b[i,j]:=putere(-1,i+j);
    m:=0;
    n:=0;
    for k:=1 to 3 do
     begin
      if k<>i then
               begin
                inc(m);
                n:=0;
               end;
      for l:=1 to 3 do
       if (k<>i) and (l<>j) then
                             begin
                              inc(n);
                              c[m,n]:=a[k,l];
                             end;
     end;
    b[i,j]:=b[i,j]*(c[1,1]*c[2,2]-c[2,1]*c[1,2]);
   end;
 writeln('Matricea adjuncta este : ');
 for i:=1 to 3 do
  begin
   writeln;
   for j:=1 to 3 do
    write(b[i,j]:3);
  end;
 readkey;
end.