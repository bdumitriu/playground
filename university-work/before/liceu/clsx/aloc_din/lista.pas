Program lista_simplu_inlantuita1;
uses crt;
type reper=^nr;
     nr=record
         inf:integer;
         leg:reper;
        end;
var c:char;
    cap:reper;
    n:integer;

Procedure creare;
var p,q:reper;
    x,i,a:integer;
begin
 write('Cate elemente doriti sa introduceti ? : ');readln(a);
 i:=1;
 repeat
  write('Elementul ',i,' : ');readln(x);
  i:=i+1;
  if cap=nil then
              begin
               new(p);
               p^.leg:=nil;
               p^.inf:=x;
               cap:=p;
              end
             else
              begin
               p:=cap;
               while p^.leg<>nil do
                p:=p^.leg;
               new(q);
               p^.leg:=q;
               q^.leg:=nil;
               q^.inf:=x;
              end;
 until i=a+1;
 n:=n+a;
end;

Procedure listare;
var p:reper;
begin
 p:=cap;
 if n>0 then
  begin
   repeat
    write(p^.inf,' ');
    p:=p^.leg;
   until p=nil;
   writeln;
  end;
 if n=0 then writeln('Nu exista elemente in lista.');
end;

Procedure adaugare;
var p,q,m:reper;
    med,min:integer;
begin
 p:=cap;
 med:=0;
 min:=p^.inf;
 repeat
  med:=med+p^.inf;
  if min>p^.inf then min:=p^.inf;
  p:=p^.leg;
 until p=nil;
 med:=med div n;
 p:=cap;
 repeat
  q:=p^.leg;
  if p^.inf=min then
                 begin
                  new(m);
                  m^.leg:=p^.leg;
                  p^.leg:=m;
                  m^.inf:=med;
                  n:=n+1;
                  p:=m^.leg;
                 end
                else p:=p^.leg;
 until p=nil;
end;

Procedure stergere;
label 1;
var p,q,m:reper;
    max:integer;
begin
 max:=0;
 p:=cap;
 repeat
  if p^.inf>max then
                 begin
                  max:=p^.inf;
                  {q:=p;}
                 end;
  p:=p^.leg;
 until p=nil;
1:p:=cap;
 if n>1 then
  if max=cap^.inf then
                   begin
                    cap:=cap^.leg;
                    dispose(p);
                    n:=n-1;
                    goto 1;
                   end
                  else
                   begin
                    m:=cap;
                    repeat
                     q:=p^.leg;
                     if q^.inf=max then
                                    begin
                                     p^.leg:=q^.leg;
                                     dispose(q);
                                     n:=n-1;
                                    end
                                   else p:=p^.leg;
                    until p=nil;
                   end
        else if n=1 then
                     begin
                      dispose(p);
                      n:=n-1;
                      cap:=nil;
                     end
                    else if n=0 then writeln('Nu avem ce sterge.');
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Pentru comanda tastati : ');
 writeln('c - pentru creare de lista sau adaugare de noi elemente.');
 writeln('l - pentru listarea elementelor din lista.');
 writeln('a - pentru adaugarea mediei aritmetice a tuturor elementelor din');
 writeln('    lista dupa elementul minim al listei.');
 writeln('s - pentru stergerea elementului(lor) maxim(e) din lista.');
 writeln('e - pentru sfarsitul programului.');
 writeln;
 cap:=nil;
 n:=0;
 repeat
  textcolor(lightblue);
  write('Comanda : ');readln(c);
  textcolor(yellow);
  case c of
   'c':creare;
   'l','e':listare;
   'a':adaugare;
   's':stergere;
  end;
 until c='e';
 readkey;
end.