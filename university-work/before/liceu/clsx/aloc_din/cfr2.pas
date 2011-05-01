Program cfr2;
uses crt;
type reper=^locom;
     locom=record
            leg:reper;
            cod:byte;
           end;
var c:char;
    prim,ultim:reper;

Procedure intrare;
var p:reper;
    x:byte;
begin
 write('Codul locomotivei care doriti sa intre : ');
 readln(x);
 if prim=nil then
              begin
               new(p);
               p^.cod:=x;
               p^.leg:=nil;
               prim:=p;
               ultim:=p;
              end
             else
              begin
               new(p);
               p^.cod:=x;
               p^.leg:=nil;
               ultim^.leg:=p;
               ultim:=p;
              end;
end;

Procedure iesire;
var p:reper;
begin
 if prim=nil then writeln('Nu exista locomotive in depou.')
             else
              begin
               writeln('Iese locomotiva ',prim^.cod);
               p:=prim;
               prim:=p^.leg;
               dispose(p);
              end;
end;

Procedure listare;
var p:reper;
begin
 if prim=nil then writeln('Nu exista locomotive in depou.')
             else
              begin
               write('Locomotivele : ');
               p:=prim;
               repeat
                write(p^.cod,' ');
                p:=p^.leg;
               until p=nil;
               writeln;
              end;
end;

begin
 clrscr;
 writeln;
 prim:=nil;
 repeat
  write('Comanda dvs : ');readln(c);
  case c of
    'i','I':intrare;
    'e','E':iesire;
    'l','L','s','S':listare;
   end;
 until (c='s') or (c='S');
 readkey;
end.