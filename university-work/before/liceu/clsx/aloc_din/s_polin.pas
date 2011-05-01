Program suma_polinoame;
uses crt;
type reper=^elem;
     elem=record
           leg:reper;
           e1,e2,c:integer;
          end;
var cap1,cap2,cap3:reper;

Procedure santinele;
begin
 new(cap1);
 new(cap2);
 new(cap3);
 cap1^.leg:=nil;
 cap2^.leg:=nil;
 cap3^.leg:=nil;
end;

Procedure creare_1_si_2;
var p,q:reper;
    x,y,z:integer;
begin
 writeln('Introduceti primul polinom : ');
 write('Introduceti exponentul lui x : ');readln(x);
 write('Introduceti exponentul lui y : ');readln(y);
 write('Introduceti coeficientul termenului actual : ');readln(z);
 p:=cap1;
 while x>-1 do
  begin
   new(q);
   q^.leg:=nil;
   q^.e1:=x;
   q^.e2:=y;
   q^.c:=z;
   p^.leg:=q;
   p:=q;
   write('Introduceti exponentul lui x : ');readln(x);
   write('Introduceti exponentul lui y : ');readln(y);
   write('Introduceti coeficientul termenului actual : ');readln(z);
  end;
 writeln('Introduceti cel de-al doilea polinom : ');
 write('Introduceti exponentul lui x : ');readln(x);
 write('Introduceti exponentul lui y : ');readln(y);
 write('Introduceti coeficientul termenului actual : ');readln(z);
 p:=cap2;
 while x>-1 do
  begin
   new(q);
   q^.leg:=nil;
   q^.e1:=x;
   q^.e2:=y;
   q^.c:=z;
   p^.leg:=q;
   p:=q;
   write('Introduceti exponentul lui x : ');readln(x);
   write('Introduceti exponentul lui y : ');readln(y);
   write('Introduceti coeficientul termenului actual : ');readln(z);
  end;
end;

Procedure creare_3;
var p1,p2,p3,q,r:reper;
    test:boolean;
begin
 p1:=cap1^.leg;
 p3:=cap3;
 repeat
  test:=true;
  p2:=cap2^.leg;
  repeat
   if (p1^.e1=p2^.e1) and (p1^.e2=p2^.e2) then
                                           begin
                                            new(q);
                                            q^.leg:=nil;
                                            q^.e1:=p1^.e1;
                                            q^.e2:=p1^.e2;
                                            q^.c:=p1^.c+p2^.c;
                                            p3^.leg:=q;
                                            p3:=q;
                                            test:=false;
                                           end;
   p2:=p2^.leg;
  until p2=nil;
  if test then
           begin
            new(q);
            q^.leg:=nil;
            q^.e1:=p1^.e1;
            q^.e2:=p1^.e2;
            q^.c:=p1^.c;
            p3^.leg:=q;
            p3:=q;
           end;
  p1:=p1^.leg;
 until p1=nil;
 p2:=cap2;
 repeat
  test:=true;
  r:=cap3;
  repeat
   if (p2^.e1=r^.e1) and (p2^.e2=r^.e2) then test:=false;
   r:=r^.leg;
  until r=nil;
  if test then
           begin
            new(q);
            q^.leg:=nil;
            q^.e1:=p2^.e1;
            q^.e2:=p2^.e2;
            q^.c:=p2^.c;
            p3^.leg:=q;
            p3:=q;
           end;
  p2:=p2^.leg;
 until p2=nil;
 repeat
  test:=true;
  p1:=cap3;
  p2:=p1^.leg;
  p3:=p2^.leg;
  repeat
   if p2^.e1<p3^.e1 then
                     begin
                      p2^.leg:=p3^.leg;
                      p3^.leg:=p2;
                      p1^.leg:=p3;
                      test:=false;
                      p2:=p1^.leg;
                      p3:=p2^.leg;
                     end;
   if p2^.e1=p2^.e1 then
                     if p2^.e2<p2^.e2 then
                                       begin
                                        p2^.leg:=p3^.leg;
                                        p3^.leg:=p2;
                                        p1^.leg:=p3;
                                        test:=false;
                                        p2:=p1^.leg;
                                        p3:=p2^.leg;
                                       end;
   p1:=p1^.leg;
   p2:=p2^.leg;
   p3:=p3^.leg;
  until p3=nil;
 until test;
 p3:=cap3^.leg;
 repeat
  write(p3^.c,'x',p3^.e1,'y',p3^.e2,' ');
  p3:=p3^.leg;
 until p3=nil;
end;

begin
 clrscr;
 writeln;
 santinele;
 creare_1_si_2;
 creare_3;
 readkey;
end.