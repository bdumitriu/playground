Program ecuatie;
uses crt;
var a,b,c,d,x1,x2:real;
begin
Writeln('Acesta este un program pt aflarea solutiilor ecuatiei de gradul 2');
Writeln('Ecuatia de gradul 2 are urmatoarea forma:ax*x+bx+c=0');
Writeln('Introduceti valorile a,b,c');
Write('a=');read(a);
Write('b=');read(b);
Write('c=');read(c);
if a<>0 then
            begin
            d:=(b*b)-(4*a*c);
            if d>0 then
                       begin
                       x1:=((-b+(sqrt(d)))/(2*a));
                       x2:=((-b-(sqrt(d)))/(2*a));
                       writeln('x1=',x1);
                       writeln('x2=',x2);
                       end
                   else if d=0 then
                                   begin
                                   x1:=(-b)/(2*a);
                                   writeln('x1=x2=',x1);
                                   end
                               else
                               writeln('x1 si x2 apartin numerelor complexe');
             end
        else
            begin
            writeln('Ecuatia devine ecuatie de gradul 1');
            if b<>0 then
                        begin
                        x1:=((-c)/b);
                        writeln('x=',x1);
                        end
                     else
                         begin
                         writeln('Ecuatia nu are solutie');
                         end;
             end;
While not KeyPressed do;
readln;
end.