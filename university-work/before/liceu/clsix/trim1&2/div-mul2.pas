Program cmmdc;
uses crt;
label 100,200;
var c,m,a1,b1,a,b,r:real;
begin
writeln('Acesta este un program de aflare a celui mai mare divizor comun si');
writeln('a celui mai mic multiplu comun a doua numere');
writeln('Intoduceti numerele');
write('a=');read(a);
write('b=');read(b);
a1:=a;
b1:=b;
if a1>=b1 then begin
100:           r:=(a-(b*(trunc(a/b))));
               if r=0 then begin
                           m:=(a1*b1)/b;
                           writeln('C.m.m.d.c.=',b);
                           writeln('C.m.m.m.c.=',m);
                           end
                      else begin
                           a:=b;
                           b:=r;
                           goto 100;
                           end;
               end
          else begin
200:           r:=(b-(a*(trunc(b/a))));
               if r=0 then begin
                           m:=(a1*b1)/a;
                           writeln('C.m.m.d.c.=',a);
                           writeln('C.m.m.m.c.=',m);
                           end
                      else begin
                           b:=a;
                           a:=r;
                           goto 200;
                           end;
               end;
while not keypressed do;
readln;
end.