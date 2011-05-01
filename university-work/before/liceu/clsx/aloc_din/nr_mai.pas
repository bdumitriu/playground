Program stive_cu_numere_mai_mici_mai_mari;
uses crt;
type reper=^elem;
     elem=record
           nr:integer;
           leg:reper;
          end;
var varfm,varfn,varfp,m,n,p,q:reper;
    i,z,nr,a:integer;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 write('Cate elemente are stiva ? ');readln(nr);
 i:=1;
 varfm:=nil;
 repeat
  write('Elementul ',i,' : ');readln(z);
  i:=i+1;
  new(m);
  m^.nr:=z;
  m^.leg:=varfm;
  varfm:=m;
 until i=nr+1;
 q:=varfm;
 write('Numarul de comparat : ');readln(a);
 varfn:=nil;varfp:=nil;
 repeat
  if q^.nr<=a then
               begin
                new(n);
                n^.nr:=q^.nr;
                n^.leg:=varfn;
                varfn:=n;
               end
              else
               begin
                new(p);
                p^.nr:=q^.nr;
                p^.leg:=varfp;
                varfp:=p;
               end;
  q:=q^.leg;
 until q=nil;
 writeln;
 if varfn=nil then writeln('Nu sunt numere mai mici decat ',a,'.')
              else
               begin
                writeln('Numerele mai mici sau egale cu ',a,' : ');
                q:=varfn;
                repeat
                 writeln(q^.nr);
                 q:=q^.leg
                until q=nil;
               end;
 writeln;
 if varfp=nil then writeln('Nu sunt numere mai mari decat ',a,'.')
              else
               begin
                writeln('Numerele mai mari decat ',a,' : ');
                q:=varfp;
                repeat
                 writeln(q^.nr);
                 q:=q^.leg
                until q=nil;
               end;
 readkey;
end.