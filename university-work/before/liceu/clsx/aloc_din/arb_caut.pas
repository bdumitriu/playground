Program arbori_de_cautare;
uses crt;
type reper=^elem;
     elem=record
           inf,fr:integer;
           s,d:reper;
          end;
var rad:reper;

Procedure creare;
var n:integer;
    p,q:reper;
    test:boolean;
begin
 rad:=nil;
 write('Elementul : ');
 readln(n);
 while n<>0 do
  begin
  if rad=nil then
              begin
               new(rad);
               rad^.s:=nil;
               rad^.d:=nil;
               rad^.inf:=n;
               rad^.fr:=1;
              end
             else
              begin
               test:=true;
               p:=rad;
               while test do
                begin
                 if n<p^.inf then
                              if p^.s=nil then
                                           begin
                                            new(q);
                                            q^.inf:=n;
                                            q^.fr:=1;
                                            q^.s:=nil;
                                            q^.d:=nil;
                                            p^.s:=q;
                                            test:=false;
                                           end
                                          else p:=p^.s;
                 if n>p^.inf then
                              if p^.d=nil then
                                           begin
                                            new(q);
                                            q^.inf:=n;
                                            q^.s:=nil;
                                            q^.d:=nil;
                                            q^.fr:=1;
                                            p^.d:=q;
                                            test:=false;
                                           end
                                          else p:=p^.d;
                 if n=p^.inf then
                              begin
                               inc(p^.fr);
                               test:=false;
                              end;
                end;
              end;
  write('Elementul : ');
  readln(n);
  end;
end;

Procedure listare(p:reper);
begin
 if p<>nil then
            begin
             write('Elementul ');
             write(p^.inf,' ');
             write('cu frecvanta ');
             writeln(p^.fr);
             listare(p^.s);
             listare(p^.d);
            end;
end;

Procedure cautare;
var p:reper;
    n:integer;
    test:boolean;
begin
 p:=rad;
 write(' Elementul pe care doriti sa-l gasiti : ');readln(n);
 test:=true;
 if n=rad^.inf then
                begin
                 writeln('Exista.');
                 test:=false;
                end
               else
                begin
                 repeat
                  if n<rad^.inf then
                                 begin
                                  p:=p^.s;
                                  if p^.inf=n then
                                               begin
                                                write('Exista.');
                                                test:=false;
                                               end;
                                 end
                                else
                                 begin
                                  p:=p^.d;
                                  if p^.inf=n then
                                               begin
                                                write('Exista.');
                                                test:=false;
                                               end;
                                 end;
                 until (test=false) or (p=nil);
                 if test then write('Nu exista.');
                end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightblue);
 creare;
 writeln;
 listare(rad);
 writeln;
 cautare;
 readkey;
end.