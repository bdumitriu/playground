Program arbore_de_cautare_creat_recursiv;
uses crt;
type reper=^elem;
     elem=record
           inf,fr:integer;
           s,d:reper;
          end;
var rad:reper;
    n:integer;
    test:boolean;

Procedure creare(var p:reper);
begin
 if p=nil then
           begin
            new(p);
            p^.inf:=n;
            p^.s:=nil;
            p^.d:=nil;
            p^.fr:=1;
           end
          else
           begin
            if n<p^.inf then creare(p^.s);
            if n>p^.inf then creare(p^.d);
            if n=p^.inf then inc(p^.fr);
           end;
end;

Procedure cautare(p:reper);
begin
 if p<>nil then
  if p^.inf=n then
               begin
                write('Exista.');
                test:=true;
               end
              else
               begin
                if n<p^.inf then cautare(p^.s);
                if n>p^.inf then cautare(p^.d);
               end;
end;


Procedure listare(p:reper);
begin
 if p<>nil then
            begin
             write('Elemntul ');
             write(p^.inf,' ');
             write('cu frecventa ');
             writeln(p^.fr,'.');
             listare(p^.s);
             listare(p^.d);
            end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightblue);
 write('Elementul : ');
 readln(n);
 while n<>0 do
  begin
   creare(rad);
   write('Elementul : ');
   readln(n);
  end;
 writeln;
 listare(rad);
 writeln;
 write(' Elementul pe care doriti sa-l cautati : ');
 readln(n);
 test:=false;
 cautare(rad);
 if test=false then write('Nu exista.');
 readkey;
end.