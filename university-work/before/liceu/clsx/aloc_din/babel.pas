Program babel_sort_cu_liste;
uses crt;
type reper=^elem;
     elem=record
           inf:integer;
           leg:reper;
          end;
var cap:reper;

Procedure sant;
begin
 new(cap);
 cap^.leg:=nil;
end;

Procedure creare;
var p,q:reper;
    n:integer;
begin
 writeln('Introduceti sirul : ');
 write('Dati primul element : ');readln(n);
 p:=cap;
 while n<>0 do
  begin
   new(q);
   q^.leg:=nil;
   q^.inf:=n;
   p^.leg:=q;
   p:=q;
   write('Dati urmatorul element : ');
   readln(n);
  end;
end;

Procedure sortare;
var p,q,r:reper;
    k:integer;
begin
 repeat
  k:=0;
  p:=cap;
  q:=p^.leg;
  r:=q^.leg;
  repeat
   if q^.inf>r^.inf then
                     begin
                      q^.leg:=r^.leg;
                      r^.leg:=q;
                      p^.leg:=r;
                      k:=1;
                      q:=p^.leg;
                      r:=q^.leg;
                     end;
   p:=p^.leg;
   q:=q^.leg;
   r:=r^.leg;
  until r=nil;
 until k=0;
end;

Procedure listare;
var p:reper;
begin
 p:=cap^.leg;
 while p<>nil do
  begin
   write(p^.inf,' ');
   p:=p^.leg;
  end;
end;

begin
 clrscr;
 writeln;
 sant;
 creare;
 sortare;
 listare;
 readkey;
end.