Program litere_cu_diez;
uses crt;
type reper=^elem;
     elem=record
           lit:char;
           leg:reper;
          end;
var cap:reper;
    c:char;

Procedure creare;
var p,q:reper;
    n:char;
begin
 new(p);
 cap:=p;
 write('Litera : ');readln(n);
 while n<>'#' do
  begin
   new(q);
   q^.lit:=n;
   q^.leg:=nil;
   p^.leg:=q;
   p:=q;
   write('Litera : ');readln(n);
  end;
end;

Procedure listare;
var p:reper;
begin
 p:=cap^.leg;
 repeat
  write(p^.lit,' ');
  p:=p^.leg;
 until p=nil;
 writeln;
end;

Procedure adaugare;
var p,q:reper;
begin
 p:=cap^.leg;
 if p^.leg<>nil then
                begin
                 p:=p^.leg;
                 new(q);
                 q^.leg:=p^.leg;
                 q^.lit:='$';
                 p^.leg:=q;
                end;
end;

Procedure stergere;
var p,q:reper;
begin
 if cap^.leg<>nil then
                   begin
                    p:=cap^.leg;
                    cap^.leg:=p^.leg;
                    dispose(p);
                   end;
end;

begin
 clrscr;
 writeln;
 write('Comanda : ');
 readln(c);
 repeat
  case c of
   'c':creare;
   'l':listare;
   'a':adaugare;
   's':stergere;
  end;
  write('Comanda : ');readln(c);
 until c='e';
 readkey;
end.