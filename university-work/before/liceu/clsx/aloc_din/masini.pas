Program masini;
uses crt;
type reper=^elem;
     elem=record
           cul,mar,nr:string;
           leg,jos:reper;
          end;
var cap1,cap2,p,q,r,s,t,u:reper;
    nr,i:integer;
    str,m,c:string;
    test:boolean;

begin
 clrscr;
 writeln;
 textcolor(lightblue);
 write('Introduceti numarul de masini : ');readln(nr);
 i:=0;
 new(cap1);
 cap1^.leg:=nil;
 p:=cap1;
 repeat
  inc(i);
  new(q);
  q^.leg:=nil;
  p^.leg:=q;
  p:=q;
  writeln('Masina ',i,' : ');
  write(' culoarea : ');readln(str);
  q^.cul:=str;
  write(' marca : ');readln(str);
  q^.mar:=str;
  write(' nr. de inmatriculare : ');readln(str);
  q^.nr:=str;
 until i=nr;
 {p:=cap1^.leg;
 repeat
  write(p^.cul,' ',p^.mar,' ',p^.nr);
  writeln;
  p:=p^.leg;
 until p=nil;}
 p:=cap1^.leg;
 new(cap2);
 cap2^.leg:=nil;
 q:=cap2;
 repeat
  test:=true;
  r:=cap2^.leg;
  while r<>nil do
   begin
    if r^.cul=p^.cul then test:=false;
    r:=r^.leg;
   end;
  if test then
   begin
    new(r);
    r^.leg:=nil;
    q^.leg:=r;
    r^.cul:=p^.cul;
    q:=r;
    s:=cap1^.leg;
    t:=r;
    repeat
     if s^.cul=r^.cul then
                       begin
                        new(u);
                        u^.jos:=nil;
                        t^.jos:=u;
                        u^.mar:=s^.mar;
                        u^.nr:=s^.nr;
                        t:=u;
                       end;
     s:=s^.leg;
    until s=nil;
   end;
  p:=p^.leg;
 until p=nil;
 textcolor(yellow);
 p:=cap2^.leg;
 writeln;
 repeat
  write('Culoarea ',p^.cul,' ');
  q:=p^.jos;
  repeat
   write(q^.mar,'/',q^.nr,' ');
   q:=q^.jos;
  until q=nil;
  writeln;
  p:=p^.leg;
 until p=nil;
 readkey;
 writeln;
 p:=cap1^.leg;
 textcolor(lightblue);
 write('Dati marca ce trebuie verificata : ');readln(m);
 write('Dati culoarea ce trebuie identificata : ');readln(c);
 writeln;
 textcolor(yellow);
 writeln('Numerele de inmatriculare a masinilor ce au marca ',m,' si culoarea',c,' sunt : ');
 repeat
  if (p^.mar=m) and (p^.cul=c) then write(p^.nr,' ');
  p:=p^.leg;
 until p=nil;
 readkey;
end.