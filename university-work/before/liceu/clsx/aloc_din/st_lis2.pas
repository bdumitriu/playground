Program please_stop_explaining;
uses crt;
type reper=^elem;
     elem=record
           urm,prec:reper;
           inf:integer;
          end;
var cap1,cap2:reper;

Procedure creare;
var n:integer;
    p,q:reper;
begin
 new(cap1);
 new(cap2);
 cap1^.urm:=cap2;
 cap2^.prec:=cap1;
 cap1^.prec:=nil;
 cap2^.urm:=nil;
 q:=cap1;
 write('Dati primul element : ');readln(n);
 while n<>0 do
  begin
   new(p);
   q^.urm:=p;
   p^.prec:=q;
   q:=p;
   p^.urm:=cap2;
   cap2^.prec:=p;
   p^.inf:=n;
   write('Dati urmatorul element : ');
   readln(n);
  end;
 write(' Lista in stare initiala : ');
 p:=cap1^.urm;
 while p<>cap2 do
  begin
   write(p^.inf,' ');
   p:=p^.urm;
  end;
 writeln;
end;

Procedure stergere;
var p,q:reper;
begin
 p:=cap1^.urm;
 while p<>cap2 do
  begin
   if p^.inf<0 then
                begin
                 q:=p^.prec;
                 p^.prec^.urm:=p^.urm;
                 p^.urm^.prec:=p^.prec;
                 dispose(p);
                 p:=q;
                end;
   p:=p^.urm;
  end;
 write(' Lista dupa stergere : ');
 p:=cap1^.urm;
 while p<>cap2 do
  begin
   write(p^.inf,' ');
   p:=p^.urm;
  end;
 writeln;
end;

begin
 clrscr;
 writeln;
 creare;
 stergere;
 readkey;
end.