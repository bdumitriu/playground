Program interclasare_a_doua_liste;
uses crt;
type reper=^elem;
     elem=record
           inf:integer;
           leg:reper;
          end;
var p,q,r,s,cap1,cap2,cap3:reper;
    a,m,n,i:integer;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce doua liste de');
 writeln('de numere ordonate crescator le va interclasa formand si afisand ');
 writeln('o a treia lista ordonata. ');
 writeln;
 textcolor(lightblue);
 write('Introduceti numarul de elemente ale primului sir : ');readln(m);
 new(cap1);
 cap1^.leg:=nil;
 p:=cap1;
 i:=0;
 while i<m do
  begin
   inc(i);
   write(' Elementul ',i,' : ');readln(a);
   new(q);
   q^.leg:=nil;
   q^.inf:=a;
   p^.leg:=q;
   p:=q;
  end;
 write('Introduceti numarul de elemente ale celui de-al doilea sir : ');
 readln(n);
 new(cap2);
 cap2^.leg:=nil;
 p:=cap2;
 i:=0;
 while i<n do
  begin
   inc(i);
   write(' Elementul ',i,' : ');readln(a);
   new(q);
   q^.leg:=nil;
   q^.inf:=a;
   p^.leg:=q;
   p:=q;
  end;
 writeln;
 textcolor(yellow);
 p:=cap1^.leg;
 q:=cap2^.leg;
 new(cap3);
 cap3^.leg:=nil;
 s:=cap3;
 repeat
  if p^.inf<=q^.inf then
                     begin
                      new(r);
                      r^.inf:=p^.inf;
                      r^.leg:=nil;
                      s^.leg:=r;
                      s:=r;
                      p:=p^.leg;
                     end
                    else
                     begin
                      new(r);
                      r^.inf:=q^.inf;
                      r^.leg:=nil;
                      s^.leg:=r;
                      s:=r;
                      q:=q^.leg;
                     end;
 until (p=nil) or (q=nil);
 if p=nil then
           repeat
            new(r);
            r^.inf:=q^.inf;
            r^.leg:=nil;
            s^.leg:=r;
            s:=r;
            q:=q^.leg;
           until q=nil
          else
           repeat
            new(r);
            r^.inf:=p^.inf;
            r^.leg:=nil;
            s^.leg:=r;
            s:=r;
            p:=p^.leg;
           until p=nil;
 writeln('Lista obtinuta este : ');
 p:=cap3^.leg;
 repeat
  write(p^.inf,' ');
  p:=p^.leg;
 until p=nil;
 readkey;
end.