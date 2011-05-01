Program cuvinte_in_ordine_alfabetica_cu_lista;
uses crt;
type reper=^elem;
     elem=record
           cuv:string;
           frecv:byte;
           leg:reper;
          end;
var p,q,m,o,r,cap:reper;
    n,i,j:integer;
    a:string;

begin
 clrscr;
 writeln;
 textcolor(lightblue);
 write('Numarul de cuvinte : ');readln(n);
 i:=1;
 cap:=nil;
 repeat
  write('Cuvantul ',i,' : ');readln(a);
  if cap=nil then
              begin
               new(p);
               cap:=p;
               p^.leg:=nil;
               p^.cuv:=a;
               p^.frecv:=1;
              end
             else
              begin
               j:=0;
               m:=cap;
               repeat
                if m^.cuv=a then
                             begin
                              m^.frecv:=m^.frecv+1;
                              m:=nil;
                              j:=1;
                             end;
                 m:=m^.leg;
               until m=nil;
               if j=0 then
                       begin
                        m:=cap;
                        if a<cap^.cuv then
                                       begin
                                        new(q);
                                        q^.leg:=cap;
                                        cap:=q;
                                        q^.cuv:=a;
                                        q^.frecv:=1;
                                       end
                                      else
                                       repeat
                                        q:=m^.leg;
                                        if q<>nil then if a<q^.cuv then
                                                           begin
                                                            new(p);
                                                            p^.cuv:=a;
                                                            p^.frecv:=1;
                                                            p^.leg:=m^.leg;
                                                            m^.leg:=p;
                                                            m:=nil;
                                                           end;
                                        if q=nil then
                                                  begin
                                                   new(p);
                                                   p^.leg:=nil;
                                                   p^.cuv:=a;
                                                   p^.frecv:=1;
                                                   m^.leg:=p;
                                                   m:=nil;
                                                  end;
                                        if m<>nil then m:=m^.leg;
                                       until m=nil;
                       end;
              end;
  i:=i+1;
 until i=n+1;
 writeln;
 textcolor(yellow);
 p:=cap;
 repeat
  writeln(p^.cuv,' - frecventa ',p^.frecv);
  p:=p^.leg;
 until p=nil;
 readkey;
end.