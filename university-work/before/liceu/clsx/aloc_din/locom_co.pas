Program locomotive_cu_coada;
uses crt;
type reper=^locom;
     locom=record
            leg:reper;
            cod:byte;
           end;
var prim,ultim:reper;
    c:char;

Procedure intrare;
var p:reper;
    x:byte;
begin
 write('The code of the locomotive you want to enter in the depot : ');
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
 if prim=nil then writeln('There are no locomotives in the depot.')
             else
              begin
               p:=prim;
               prim:=prim^.leg;
               writeln('The code of the locomotive that is taken out is ',p^.cod);
               dispose(p);
              end;
end;

Procedure listare;
var p:reper;
begin
 if prim=nil then writeln('There are no locomotives in the depot.')
             else
              begin
               write('The locomotives in the depot are : ');
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
  write('YOUR ORDER, SIR : ');readln(c);
  case c of
   'S','s','L','l':listare;
   'I','i':intrare;
   'E','e':iesire;
  end;
 until (c='S') or (c='s');
 readkey;
end.