Program matrici_rare;
uses crt;
type reper=^elem;
     elem=record
           lin,col,val:integer;
           jos,dr:reper;
          end;
var cap1,cap2,cap3,p,q,r,p1,q1,p2,p1a,p2a,pa:reper;
    m,n,i,j,k:integer;
    c:char;

begin
 clrscr;
 writeln;
 textcolor(lightblue);
 new(cap1);
 cap1^.lin:=-1;
 cap1^.col:=-1;
 cap1^.jos:=nil;
 cap1^.dr:=nil;
 write('Introduceti numarul de linii ale matricii : ');readln(m);
 write('Introduceti numarul de coloane ale matricii : ');readln(n);
 i:=0;
 p:=cap1;
 while i<m do
  begin
   inc(i);
   new(q);
   q^.lin:=-1;
   q^.col:=i;
   q^.dr:=cap1;
   q^.jos:=nil;
   p^.dr:=q;
   p:=q;
  end;
 p:=cap1;
 i:=0;
 while i<n do
  begin
   inc(i);
   new(q);
   q^.lin:=i;
   q^.col:=-1;
   q^.dr:=q;
   q^.jos:=cap1;
   p^.jos:=q;
   p:=q;
  end;
 writeln('Acum introduceti matricea unu :');
 repeat
  write(' Dati linia : ');readln(i);
  write(' Dati coloana : ');readln(j);
  write(' Dati valoarea : ');readln(k);
  p:=cap1;
  repeat
   p:=p^.jos;
  until p^.lin=i;
  p1:=p;
  while (p1^.dr<>nil) and (p1^.dr<>p) do
   p1:=p1^.dr;
  q:=cap1;
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
 new(cap2);
 cap2^.lin:=-1;
 cap2^.col:=-1;
 cap2^.jos:=nil;
 cap2^.dr:=nil;
 write('Introduceti numarul de linii ale matricii : ');readln(m);
 write('Introduceti numarul de coloane ale matricii : ');readln(n);
 i:=0;
 p:=cap2;
 while i<m do
  begin
   inc(i);
   new(q);
   q^.lin:=-1;
   q^.col:=i;
   q^.dr:=cap2;
   q^.jos:=nil;
   p^.dr:=q;
   p:=q;
  end;
 p:=cap2;
 i:=0;
 while i<n do
  begin
   inc(i);
   new(q);
   q^.lin:=i;
   q^.col:=-1;
   q^.dr:=q;
   q^.jos:=cap2;
   p^.jos:=q;
   p:=q;
  end;
 writeln('Acum introduceti matricea doi :');
 repeat
  write(' Dati linia : ');readln(i);
  write(' Dati coloana : ');readln(j);
  write(' Dati valoarea : ');readln(k);
  p:=cap2;
  repeat
   p:=p^.jos;
  until p^.lin=i;
  p1:=p;
  while (p1^.dr<>nil) and (p1^.dr<>p) do
   p1:=p1^.dr;
  q:=cap2;
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
 p:=cap1;
 i:=0;
 repeat
  p:=p^.dr;
  inc(i);
 until p^.dr=cap1;
 p:=cap2;
 j:=0;
 repeat
  p:=p^.dr;
  inc(j);
 until p^.dr=cap2;
 if i>j then m:=i;
 if i<=j then m:=j;
 new(cap3);
 cap3^.lin:=-1;
 cap3^.col:=-1;
 cap3^.dr:=nil;
 cap3^.jos:=nil;
 i:=0;
 p:=cap3;
 while i<m do
  begin
   inc(i);
   new(q);
   q^.lin:=-1;
   q^.col:=i;
   q^.dr:=cap3;
   q^.jos:=nil;
   p^.dr:=q;
   p:=q;
  end;
 p:=cap1;
 i:=0;
 repeat
  p:=p^.jos;
  inc(i);
 until p^.jos=cap1;
 p:=cap2;
 j:=0;
 repeat
  p:=p^.jos;
  inc(j);
 until p^.jos=cap2;
 if i>j then n:=i;
 if i<=j then n:=j;
 p:=cap3;
 i:=0;
 while i<n do
  begin
   inc(i);
   new(q);
   q^.lin:=i;
   q^.col:=-1;
   q^.dr:=nil;
   q^.jos:=cap3;
   p^.jos:=q;
   p:=q;
  end;
 p1:=cap1;
 p2:=cap2;
 p:=cap3;
 repeat
  p1:=p1^.jos;
  p2:=p2^.jos;
  p:=p^.jos;
  p1a:=p1^.dr;
  p2a:=p2^.dr;
  pa:=p;
  repeat
   if (p1a<>p1) and (p2a<>p2) then
   if p1a^.col=p2a^.col then
                         begin
                          new(r);
                          r^.lin:=p1a^.lin;
                          r^.col:=p1a^.col;
                          r^.val:=p1a^.val+p2a^.val;
                          pa^.dr:=r;
                          r^.dr:=p;
                          pa:=r;
                          q:=cap3;
                          repeat
                           q:=q^.dr;
                          until q^.col=p1a^.col;
                          q1:=q;
                          while (q1^.jos<>nil) and (q1^.jos<>q) do
                           q1:=q1^.jos;
                          q1^.jos:=r;
                          r^.jos:=q;
                          p1a:=p1a^.dr;
                          p2a:=p2a^.dr;
                         end;
   if (p1a<>p1) and (p2a<>p2) then
   if p1a^.col<p2a^.col then
                         begin
                          new(r);
                          r^.lin:=p1a^.lin;
                          r^.col:=p1a^.col;
                          r^.val:=p1a^.val;
                          pa^.dr:=r;
                          r^.dr:=p;
                          pa:=r;
                          q:=cap3;
                          repeat
                           q:=q^.dr;
                          until q^.col=p1a^.col;
                          q1:=q;
                          while (q1^.jos<>nil) and (q1^.jos<>q) do
                           q1:=q1^.jos;
                          q1^.jos:=r;
                          r^.jos:=q;
                          p1a:=p1a^.dr;
                         end;
   if (p1a<>p1) and (p2a<>p2) then
   if p1a^.col>p2a^.col then
                         begin
                          new(r);
                          r^.lin:=p2a^.lin;
                          r^.col:=p2a^.col;
                          r^.val:=p2a^.val;
                          pa^.dr:=r;
                          r^.dr:=p;
                          pa:=r;
                          q:=cap3;
                          repeat
                           q:=q^.dr;
                          until q^.col=p2a^.col;
                          q1:=q;
                          while (q1^.jos<>nil) and (q1^.jos<>q) do
                           q1:=q1^.jos;
                          q1^.jos:=r;
                          r^.jos:=q;
                          p2a:=p2a^.dr;
                         end;
  until (p1a=p1) or (p2a=p2);
  if p1a=p1 then
             if p2a<>p2 then
                         repeat
                          new(r);
                          r^.lin:=p2a^.lin;
                          r^.col:=p2a^.col;
                          r^.val:=p2a^.val;
                          pa^.dr:=r;
                          r^.dr:=p;
                          pa:=r;
                          q:=cap3;
                          repeat
                           q:=q^.dr;
                          until q^.col=p2a^.col;
                          q1:=q;
                          while (q1^.jos<>nil) and (q1^.jos<>q) do
                           q1:=q1^.jos;
                          q1^.jos:=r;
                          r^.jos:=q;
                          p2a:=p2a^.dr;
                         until p2a=p2;
  if p2a=p2 then
             if p1a<>p1 then
                         repeat
                          new(r);
                          r^.lin:=p1a^.lin;
                          r^.col:=p1a^.col;
                          r^.val:=p1a^.val;
                          pa^.dr:=r;
                          r^.dr:=p;
                          pa:=r;
                          q:=cap3;
                          repeat
                           q:=q^.dr;
                          until q^.col=p1a^.col;
                          q1:=q;
                          while (q1^.jos<>nil) and (q1^.jos<>q) do
                           q1:=q1^.jos;
                          q1^.jos:=r;
                          r^.jos:=q;
                          p1a:=p1a^.dr;
                         until p1a=p1;
 until (p1^.jos=cap1) or (p2^.jos=cap2);
 if p1^.jos=cap1 then
                  if p2^.jos<>cap2 then
                    repeat
                     p2:=p2^.jos;
                     p:=p^.jos;
                     p2a:=p2^.dr;
                     pa:=p;
                     if p2a<>p2 then
                      repeat
                       new(r);
                       r^.lin:=p2a^.lin;
                       r^.col:=p2a^.col;
                       r^.val:=p2a^.val;
                       pa^.dr:=r;
                       r^.dr:=p;
                       pa:=r;
                       q:=cap3;
                       repeat
                        q:=q^.dr;
                       until q^.col=p2a^.col;
                       q1:=q;
                       while (q1^.jos<>nil) and (q1^.jos<>q) do
                        q1:=q1^.jos;
                       q1^.jos:=r;
                       r^.jos:=q;
                       p2a:=p2a^.dr;
                      until p2a=p2;
                     until p2^.jos=cap2;
 if p2^.jos=cap2 then
                  if p1^.jos<>cap1 then
                    repeat
                     new(r);
                     r^.lin:=p1a^.lin;
                     r^.col:=p1a^.col;
                     r^.val:=p1a^.val;
                     pa^.dr:=r;
                     r^.dr:=p;
                     pa:=r;
                     q:=cap3;
                     repeat
                      q:=q^.dr;
                     until q^.col=p1a^.col;
                     q1:=q;
                     while (q1^.jos<>nil) and (q1^.jos<>q) do
                      q1:=q1^.jos;
                     q1^.jos:=r;
                     r^.jos:=q;
                     p1a:=p1a^.dr;
                    until p1a=cap1;
 p:=cap3;
 repeat
  p:=p^.jos;
  q:=p^.dr;
  while (q<>nil) and (q<>p) do
   begin
    write(q^.val,' ');
    q:=q^.dr;
   end;
  writeln;
 until p^.jos=cap3;
 readkey;
end.