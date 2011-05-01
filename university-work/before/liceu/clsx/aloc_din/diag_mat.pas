Program minimul_de_pe_diagonala_principala_a_unei_matrici;
uses crt;
type reper=^elem;
     elem=record
           lin,col,val:integer;
           jos,dr:reper;
          end;
var cap,p,q,r,p1,q1:reper;
    i,j,k,m,n,min:integer;
    c:char;

begin
 clrscr;
 writeln;
 textcolor(lightblue);
 new(cap);
 cap^.lin:=-1;
 cap^.col:=-1;
 cap^.jos:=nil;
 cap^.dr:=nil;
 write('Introduceti numarul de linii ale matricii : ');readln(m);
 write('Introduceti numarul de coloane ale matricii : ');readln(n);
 i:=0;
 p:=cap;
 while i<m do
  begin
   inc(i);
   new(q);
   q^.lin:=-1;
   q^.col:=i;
   q^.dr:=cap;
   q^.jos:=q;
   p^.dr:=q;
   p:=q;
  end;
 p:=cap;
 i:=0;
 while i<n do
  begin
   inc(i);
   new(q);
   q^.lin:=i;
   q^.col:=-1;
   q^.dr:=q;
   q^.jos:=cap;
   p^.jos:=q;
   p:=q;
  end;
 writeln('Acum introduceti matricea :');
 repeat
  write(' Dati linia : ');readln(i);
  write(' Dati coloana : ');readln(j);
  write(' Dati valoarea : ');readln(k);
  p:=cap;
  repeat
   p:=p^.jos;
  until p^.lin=i;
  p1:=p;
  while (p1^.dr<>nil) and (p1^.dr<>p) do
   p1:=p1^.dr;
  q:=cap;
  repeat
   q:=q^.dr;
  until q^.col=j;
  q1:=q;
  while (q1^.jos<>nil) and (q1^.jos<>q) do
   q1:=q1^.jos;
  new(r);
  r^.lin:=i;
  r^.col:=j;
  r^.val:=k;
  r^.dr:=p;
  r^.jos:=q;
  p1^.dr:=r;
  q1^.jos:=r;
  write(' Mai doriti sa introduceti elemente (d/n) ? ');readln(c);
 until c='n';
 p:=cap^.jos;
 min:=maxint;
 repeat
  p1:=p^.dr;
  while p1<>p do
   begin
    if p1^.lin=p1^.col then if p1^.val<min then min:=p1^.val;
    p1:=p1^.dr;
   end;
  p:=p^.jos;
 until p=cap;
 writeln;
 write('Minimul de pe diagonala principala este ',min,'.');
 readkey;
end.