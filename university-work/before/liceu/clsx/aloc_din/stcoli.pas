Program numere_cu_stiva_coada_si_lista_inlantuita;
uses crt;
type reper=^elem;
     elem=record
           leg:reper;
           inf:integer;
          end;
var varf,prim,ultim,cap:reper;

Procedure creare_stiva;
var p,q:reper;
    n:integer;
begin
 writeln('Introduceti elementele din stiva : ');
 write('Dati elementul : ');readln(n);
 varf:=nil;
 while n<>0 do
  begin
   new(q);
   q^.leg:=varf;
   q^.inf:=n;
   varf:=q;
   write('Dati urmatorul element : ');readln(n);
  end;
 p:=varf;
 if p=nil then writeln('Stiva e vida.')
          else
           repeat
            write(p^.inf,' ');
            p:=p^.leg;
           until p=nil;
 readkey;
 writeln;
end;

Procedure creare_coada;
var p,q:reper;
begin
 p:=varf;
 prim:=nil;
 ultim:=nil;
 repeat
  if p^.inf mod 2 = 0 then
                       if prim=nil then
                                    begin
                                     new(q);
                                     prim:=q;
                                     q^.inf:=p^.inf;
                                     q^.leg:=nil;
                                     ultim:=q;
                                    end
                                   else
                                    begin
                                     new(q);
                                     q^.inf:=p^.inf;
                                     q^.leg:=nil;
                                     ultim^.leg:=q;
                                     ultim:=q;
                                    end;
  p:=p^.leg;
 until p=nil;
 p:=prim;
 if p=nil then writeln('Coada e vida.')
          else
           repeat
            write(p^.inf,' ');
            p:=p^.leg;
           until p=nil;
 readkey;
 writeln;
end;

Procedure creare_lista_simplu_inlantuita;
var p,q,r:reper;
begin
 new(cap);
 cap^.leg:=nil;
 p:=varf;
 r:=cap;
 repeat
  if p^.inf mod 2 = 1 then
                       begin
                        new(q);
                        q^.leg:=nil;
                        r^.leg:=q;
                        r:=q;
                        q^.inf:=p^.inf;
                       end;
  p:=p^.leg;
 until p=nil;
 p:=cap^.leg;
 if p=nil then writeln('Lista e vida.')
          else
           repeat
            write(p^.inf,' ');
            p:=p^.leg;
           until p=nil;
 readkey;
end;

Procedure stergere_stiva;
var p,q:reper;
begin
 p:=varf;
 repeat
  q:=p^.leg;
  dispose(p);
  p:=q;
 until q=nil;
 varf:=nil;
end;

begin
 clrscr;
 writeln;
 creare_stiva;
 creare_coada;
 creare_lista_simplu_inlantuita;
 stergere_stiva;
end.