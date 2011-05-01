Program liste_cu_cifre;
uses crt;
type reper=^elem;
     elem=record
           urm,prec:reper;
           inf:integer;
          end;
var cap1a,cap1b,cap2a,cap2b,cap3a,cap3b,p,q,r,s:reper;
    i,tr,code:integer;
    nr1,nr2:string;

begin
 clrscr;
 writeln;
 textcolor(lightblue);
 write('Introduceti primul numar : ');readln(nr1);
 write('Introduceti al doilea numar : ');readln(nr2);
 new(cap1a);
 new(cap1b);
 cap1a^.urm:=cap1b;
 cap1a^.prec:=nil;
 cap1b^.prec:=cap1a;
 cap1b^.urm:=nil;
 cap1a^.inf:=0;
 p:=cap1a;
 i:=0;
 repeat
  new(q);
  q^.prec:=p;
  q^.urm:=cap1b;
  p^.urm:=q;
  cap1b^.prec:=q;
  inc(i);
  val(nr1[i],q^.inf,code);
  p:=q;
 until i=length(nr1);
 new(cap2a);
 new(cap2b);
 cap2a^.urm:=cap2b;
 cap2a^.prec:=nil;
 cap2b^.prec:=cap2a;
 cap2b^.urm:=nil;
 cap2a^.inf:=0;
 p:=cap2a;
 i:=0;
 repeat
  new(q);
  q^.prec:=p;
  q^.urm:=cap2b;
  p^.urm:=q;
  cap2b^.prec:=q;
  inc(i);
  val(nr2[i],q^.inf,code);
  p:=q;
 until i=length(nr2);
 {p:=cap1a^.urm;
 repeat
  write(p^.inf);
  p:=p^.urm;
 until p=cap1b;}
 p:=cap1b^.prec;
 q:=cap2b^.prec;
 new(cap3a);
 new(cap3b);
 cap3a^.urm:=cap3b;
 cap3a^.prec:=nil;
 cap3b^.prec:=cap3a;
 cap3b^.urm:=nil;
 s:=cap3b;
 tr:=0;
 repeat
  new(r);
  r^.urm:=s;
  r^.prec:=cap3a;
  cap3a^.urm:=r;
  s^.prec:=r;
  s:=r;
  r^.inf:=(p^.inf+q^.inf+tr) mod 10;
  tr:=(p^.inf+q^.inf+tr) div 10;
  if p<>cap1a then p:=p^.prec;
  if q<>cap2a then q:=q^.prec;
 until (p=cap1a) and (q=cap2a);
 p:=cap3a^.urm;
 textcolor(yellow);
 write('Suma este : ');
 repeat
  write(p^.inf);
  p:=p^.urm;
 until p=cap3b;
 readkey;
end.