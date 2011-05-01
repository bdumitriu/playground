Program ciurul_lui_Eratostene;
uses crt;
type reper=^elem;
     elem=record
           inf:integer;
           leg:reper;
          end;
var n:integer;
    cap:reper;

Procedure santinela;
begin
 new(cap);
 cap^.leg:=nil;
end;

Procedure creare;
var p,q,r:reper;
    i:integer;
begin
 new(p);
 p^.inf:=2;
 p^.leg:=nil;
 cap^.leg:=p;
 p:=cap^.leg;
 i:=3;
 repeat
  new(q);
  q^.inf:=i;
  q^.leg:=nil;
  p^.leg:=q;
  p:=q;
  i:=i+1;
 until i=n+1;
 i:=1;
 r:=cap^.leg;
 while r^.leg<>nil do
  begin
   i:=r^.inf;
   q:=r;
   p:=r^.leg;
   repeat
    if p^.inf mod i = 0 then
                         begin
                          q^.leg:=p^.leg;
                          dispose(p);
                          p:=q^.leg;
                         end
                        else
                         begin
                          p:=p^.leg;
                          q:=q^.leg;
                         end;
   until p=nil;
   r:=r^.leg;
  end;
end;

Procedure listare;
var p:reper;
begin
 p:=cap^.leg;
 repeat
  write(p^.inf,' ');
  p:=p^.leg;
 until p=nil;
end;

begin
 clrscr;
 writeln;
 write('Valoarea maxima pana la cat sa se afiseze numerele prime : ');
 readln(n);
 if n=1 then writeln('Nu exista numere prime pana la 1.');
 if n=2 then writeln('2');
 if n>2 then
  begin
   santinela;
   creare;
   listare;
  end;
 readkey;
end.