Program cmmdc;
uses crt;
var c,m,a1,b1,a,b,r:integer;
begin
clrscr;
writeln('Acesta este un program de aflare a celui mai mare divizor comun si');
writeln('a celui mai mic multiplu comun a doua numere');
writeln('Intoduceti numerele');
write('a=');read(a);
write('b=');read(b);
a1:=a;
b1:=b;
if a1>=b1 then begin
               r:=(a-(b*(a div b)));
               if r<>0 then begin
                            repeat
                            begin
                            a:=b;
                            b:=r;
                            r:=(a-(b*(a div b)));
                            end;
                            until r=0;
                            m:=(a1*b1) div b;
                            writeln('C.m.m.d.c.=',b);
                            writeln('C.m.m.m.c.=',m);
                            end
                       else
                            begin
                            m:=(a1*b1) div b;
                            writeln('C.m.m.d.c.=',b);
                            writeln('C.m.m.m.c.=',m);
                            end;
               end
          else begin
               r:=(b-(a*(b div a)));
               if r<>0 then begin
                            repeat
                            begin
                            b:=a;
                            a:=r;
                            r:=(b-(a*(b div a)));
                            end;
                            until r=0;
                            m:=(b1*a1) div a;
                            writeln('c.m.m.d.c.=',a);
                            writeln('c.m.m.m.c.=',m);
                            end
                       else begin
                            m:=(b1*a1) div a;
                            writeln('c.m.m.d.c.=',a);
                            writeln('c.m.m.m.c.=',m);
                            end;
               end;
while not keypressed do;
readln;
end.