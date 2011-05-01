Program masini_cu_arbore;
uses crt;
type reper=^elem;
     elem=record
           s,d:reper;
           ma:string;
           nr:integer;
          end;
var rad,p,tp,q,tq:reper;
    m:string;
    i,n,nr:integer;
    c,d:char;
    x:array[1..50] of integer;
    test:boolean;

Procedure creare(var p:reper);
begin
 if p=nil then
           begin
            new(p);
            p^.s:=nil;
            p^.d:=nil;
            p^.ma:=m;
            p^.nr:=nr;
           end
          else
           begin
            if nr<p^.nr then creare(p^.s);
            if nr>p^.nr then creare(p^.d);
           end;
end;

Procedure listare(p:reper);
begin
 if p<>nil then
            begin
             writeln('   ',p^.ma,' - nr. ',p^.nr);
             listare(p^.s);
             listare(p^.d);
            end;
end;

Procedure cautare_m(p:reper);
begin
 if p<>nil then
            begin
             if p^.ma=m then
                         begin
                          inc(i);
                          x[i]:=p^.nr;
                         end;
             cautare_m(p^.s);
             cautare_m(p^.d);
            end;
end;

Procedure cautare_n(p:reper);
begin
 if p<>nil then
            begin
             if p^.nr=nr then
                          begin
                           test:=false;
                           writeln(p^.ma,'.');
                          end;
             cautare_n(p^.s);
             cautare_n(p^.d);
            end;
end;

Procedure stergere;

Procedure sterg;
begin
 if (p^.s=nil) and (p^.d=nil) then
                               if p=rad then rad:=nil
                                        else
                                         begin
                                          if tp^.s=p then tp^.s:=nil;
                                          if tp^.d=p then tp^.d:=nil;
                                          dispose(p);
                                         end
 else if (p^.s=nil) and (p^.d<>nil) then
                                     if p=rad then rad:=rad^.d
                                              else
                                               begin
                                                if tp^.s=p then tp^.s:=p^.d;
                                                if tp^.d=p then tp^.d:=p^.d;
                                                dispose(p);
                                               end
 else if (p^.s<>nil) and (p^.d=nil) then
                                     if p=rad then rad:=rad^.s
                                              else
                                               begin
                                                if tp^.s=p then tp^.s:=p^.s;
                                                if tp^.d=p then tp^.d:=p^.s;
                                                dispose(p);
                                               end
 else if (p^.s<>nil) and (p^.d<>nil) then
                                      begin
                                       tq:=p;
                                       q:=p^.d;
                                       while q^.s<>nil do
                                        q:=q^.s;
                                       p^.ma:=q^.ma;
                                       p^.nr:=q^.nr;
                                       if tq=p then
                                                begin
                                                 p^.d:=q^.d;
                                                end
                                               else
                                                begin
                                                 tq^.s:=q^.d;
                                                end;
                                       dispose(q);
                                      end;
end;

Procedure caut;
var test:boolean;
begin
 tp:=rad;
 p:=rad;
 test:=false;
 repeat
  if p^.nr=x[n] then
                 begin
                  if p=rad then tp:=nil;
                  test:=true;
                  sterg;
                 end;
  if p^.nr>x[n] then
                 begin
                  tp:=p;
                  p:=p^.s;
                 end
                else
                 begin
                  tp:=p;
                  p:=p^.d;
                 end;
 until (test) or (p=nil);
end;

begin
 for i:=1 to 50 do
  x[i]:=0;
 i:=0;
 cautare_m(rad);
 n:=0;
 while n<i do
  begin
   inc(n);
   caut;
  end;
end;

begin
 clrscr;
 writeln;
 textcolor(yellow);
 writeln(' Primul pas este crearea initiala a arborelui.');
 write('  Introduceti numarul de masini pe care le doriti in arbore : ');
 readln(n);
 i:=0;
 textcolor(lightblue);
 rad:=nil;
 while i<n do
  begin
   inc(i);
   write('   marca masinii ',i,' : ');
   readln(m);
   write('   numarul de inmatriculare al masinii ',i,' : ');
   readln(nr);
   creare(rad);
  end;
 textcolor(yellow);
 writeln(' Pentru a verifica crearea corecta a arborelui aceasta este lista');
 writeln(' elementelor din acesta : ');
 listare(rad);
 writeln(' Dupa ce ati terminat verificarea apsati va rog "ENTER".');
 readln;
 clrscr;
 writeln(' In continuare puteti executa una din urmatoarele comenzi : ');
 writeln('  a - adaugarea unei noi masini in arbore.');
 writeln('  l - listarea arborelui in starea actuala.');
 writeln('  s - stergerea masinilor cu marca data de dvs. din arbore.');
 writeln('  c - cautarea unui element in arbore.');
 writeln('  e - terminarea programului.');
 writeln;
 textcolor(lightblue);
 write(' Dorinta dvs. : ');
 readln(c);
 while (c<>'e') and (c<>'E') do
  begin
   case c of
    'a','A' : begin
               write('   marca masinii : ');
               readln(m);
               write('   numarul de inmatriculare al masinii : ');
               readln(nr);
               creare(rad);
              end;
    'c','C' : begin
               write(' Doriti sa cautati masina dupa marca sau dupa numar (m/n) : ');
               readln(d);
               if (d='m') or (d='M') then
                                      begin
                                       write('  marca : ');
                                       readln(m);
                                       for i:=1 to 50 do
                                        x[i]:=0;
                                       i:=0;
                                       cautare_m(rad);
                                       writeln(' Masinile ',m,' au numerele : ');
                                       for n:=1 to i do
                                        writeln('   ',x[n]);
                                       if i=0 then
                                               begin
                                                gotoxy(1,wherey-1);
                                                delline;
                                                gotoxy(1,wherey);
                                                writeln(' Nu exista masini cu marca ',m,'.');
                                               end;
                                      end
                                     else
                                      begin
                                       write('  numarul : ');
                                       readln(nr);
                                       test:=true;
                                       write(' Masina cu numarul ',nr,' are marca ');
                                       cautare_n(rad);
                                       if test then
                                                begin
                                                 gotoxy(1,wherey-1);
                                                 delline;
                                                 gotoxy(1,wherey);
                                                 writeln(' Nu exista masina cu numarul ',nr,'.');
                                                end;
                                      end;
              end;
    'l','L' : begin
               writeln('  Acestea sunt elementele din arbore in acest moment : ');
               listare(rad);
              end;
    's','S' :  begin
                write('   marca pe care doriti sa o stergeti : ');
                readln(m);
                stergere;
               end;
   end;
   write(' Dorinta dvs. : ');
   readln(c);
  end;
end.