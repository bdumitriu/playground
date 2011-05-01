Program stive_cu_suma_cifrelor_divizibila_si_produsul_cifrelor_divizibil;
uses crt;
type reper=^elem;
     elem=record
           nr:integer;
           leg:reper;
          end;
var n,i,x,a,j:integer;
    p,m,o,varf1,varf2,varf3:reper;

Function suma(z:integer):integer;
begin
 if trunc(z/10)=0 then suma:=z
                  else suma:=suma(trunc(z/10))+z mod 10;
end;

Function produs(z:integer):integer;
begin
 if trunc(z/10)=0 then produs:=z
                  else produs:=produs(trunc(z/10))*(z mod 10);
end;


begin
 clrscr;
 writeln;
 write('Numarul de elemente al stivei initiale : ');readln(n);
 i:=1;
 varf1:=nil;
 repeat
  write('Numarul ',i,' : ');readln(x);
  i:=i+1;
  new(p);
  p^.nr:=x;
  p^.leg:=varf1;
  varf1:=p;
 until i=n+1;
 write('Numarul cu care trebuie sa fie divizibil : ');readln(a);
 p:=varf1;
 varf2:=nil;
 varf3:=nil;
 repeat
  if suma(p^.nr) mod a = 0 then
                            begin
                             new(m);
                             m^.nr:=p^.nr;
                             m^.leg:=varf2;
                             varf2:=m;
                            end;
  if produs(p^.nr) mod a = 0 then
                              begin
                               new(o);
                               o^.nr:=p^.nr;
                               o^.leg:=varf3;
                               varf3:=o;
                              end;
  p:=p^.leg;
 until p=nil;
 m:=varf2;
 o:=varf3;
 writeln('Stiva cu numerele a caror suma a cifrelor e divizibila cu ',a,' : ');
 repeat
  write(m^.nr,' ');
  m:=m^.leg;
 until m=nil;
 writeln;
 writeln('Stiva cu numerele a caror produs al cifrelor e divizibil cu ',a,' : ');
 repeat
  write(o^.nr,' ');
  o:=o^.leg;
 until o=nil;
 readkey;
end.