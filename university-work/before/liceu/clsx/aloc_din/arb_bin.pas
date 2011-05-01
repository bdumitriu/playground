Program arbore_binar_creare_etc;
uses crt;
type reper=^nod;
     nod=record
          s,d:reper;
          nr:integer;
         end;
var cap:reper;

Procedure creare(var p:reper);
var n:integer;
begin
 write('Varful : ');
 readln(n);
 if n<>0 then
          begin
           new(p);
           p^.nr:=n;
           creare(p^.s);
           creare(p^.d);
          end
         else p:=nil;
end;

Procedure listare1(p:reper);
begin
 if p<>nil then
            begin
             write(p^.nr,' ');
             listare1(p^.s);
             listare1(p^.d);
            end;
end;

Procedure listare2(p:reper);
begin
 if p<>nil then
            begin
             listare2(p^.s);
             write(p^.nr,' ');
             listare2(p^.d);
            end;
end;

Procedure listare3(p:reper);
begin
 if p<>nil then
            begin
             listare3(p^.s);
             listare3(p^.d);
             write(p^.nr,' ');
            end;
end;

begin
 clrscr;
 writeln;
 creare(cap);
 write(' Preordine : ');
 listare1(cap);
 writeln;
 write(' Inordine : ');
 listare2(cap);
 writeln;
 write(' Postordine : ');
 listare3(cap);
 writeln;
 readkey;
end.