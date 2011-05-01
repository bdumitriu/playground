Program cfr1;
uses crt;
type reper=^loco;
     loco=record
           nr:integer;
           leg:reper;
          end;
var varf,p:reper;
    c:char;

Procedure intrare;
var q:reper;
    z:integer;
begin
 write('Numarul locomotivei care intra : ');readln(z);
 new(p);
 p^.nr:=z;
 p^.leg:=varf;
 varf:=p;
end;

Procedure iesire;
var q:reper;
    z:integer;
begin
 repeat
  write('Numarul locomotivei care iese : ');readln(z);
 until z=varf^.nr;
 q:=varf;
 varf:=q^.leg;
 dispose(q);
end;

Procedure listare;
var q:reper;
begin
 q:=varf;
 repeat
  writeln(q^.nr);
  q:=q^.leg;
 until q=nil;
end;

begin
 clrscr;
 writeln;
 repeat
  write('Your order, SIR : ');readln(c);
  case c of
   'I','i':intrare;
   'E','e':iesire;
   'S','s','L','l':listare;
  end
 until (c='S') or (c='s');
 readkey;
end.