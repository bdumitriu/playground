Program cifre_impare_dupa_care_se_adauga_dublul_lor;
uses crt;
type reper=^elem;
     elem=record
           leg:reper;
           inf:integer;
          end;
var cap:reper;

Procedure santinela;
var p:reper;
begin
 new(p);
 cap:=p;
end;

Procedure creare;
var p,q:reper;
    n:integer;
begin
 p:=cap;
 write('Numarul : ');readln(n);
 while n<>0 do
  begin
   new(q);
   q^.inf:=n;
   q^.leg:=nil;
   p^.leg:=q;
   p:=q;
   write('Numarul : ');readln(n);
  end;
end;

Procedure listare;
var p:reper;
begin
 p:=cap^.leg;
 if p<>nil then
            begin
             repeat
              write(p^.inf,' ');
              p:=p^.leg;
             until p=nil;
             writeln;
            end;
end;

Procedure adaugare;
var p,q:reper;
begin
 p:=cap^.leg;
 if p<>nil then
            begin
             repeat
              if p^.inf mod 2 = 1 then
                                   begin
                                    new(q);
                                    q^.inf:=p^.inf*2;
                                    q^.leg:=p^.leg;
                                    p^.leg:=q;
                                    p:=q^.leg;
                                   end;
              p:=p^.leg;
             until p=nil;
            end;
end;

Procedure stergere;
var p,q:reper;
begin
 p:=cap;
 q:=cap^.leg;
 if q<>nil then
            begin
             repeat
              if q^.inf mod 2 = 0 then
                                   begin
                                    p^.leg:=q^.leg;
                                    dispose(q);
                                    q:=p^.leg;
                                   end
                                  else
                                   begin
                                    q:=q^.leg;
                                    p:=p^.leg;
                                   end;
             until q=nil;
            end;
end;

begin
 clrscr;
 creare;
 adaugare;
 stergere;
 listare;
 readkey;
end.