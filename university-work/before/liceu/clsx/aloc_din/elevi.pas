Program elevi_cu_medii_de_corigenta;
uses crt;
type reper=^elev;
     elev=record
           nr,med:integer;
           nume:string;
           s,d:reper;
          end;
var tp,p,rad:reper;
    c:string;
    n,m,i,sir:integer;
    x:array[1..10] of integer;

Procedure creare(var p:reper);
begin
 if p=nil then
           begin
            new(p);
            p^.nume:=c;
            p^.nr:=n;
            p^.med:=m;
            p^.s:=nil;
            p^.d:=nil;
           end
          else
           begin
            if n<p^.nr then creare(p^.s);
            if n>p^.nr then creare(p^.d);
           end;
end;

Procedure cautare(p:reper);
begin
 if p<>nil then
            begin
             if p^.med<5 then
                          begin
                           inc(i);
                           x[i]:=p^.nr;
                          end;
             cautare(p^.s);
             cautare(p^.d);
            end;
end;

Procedure nod_t;
begin
 if p<>rad then
  begin
   if tp^.s=p then tp^.s:=nil;
   if tp^.d=p then tp^.d:=nil;
   dispose(p);
  end;
 if p=rad then rad:=nil;
end;

Procedure un_fiu_s;
begin
 if p<>rad then
  begin
   if tp^.s=p then tp^.s:=p^.s;
   if tp^.d=p then tp^.d:=p^.s;
   dispose(p);
  end;
 if p=rad then rad:=rad^.s;
end;

Procedure un_fiu_d;
begin
 if p<>rad then
  begin
   if tp^.s=p then tp^.s:=p^.d;
   if tp^.d=p then tp^.d:=p^.d;
   dispose(p);
  end;
 if p=rad then rad:=rad^.d;
end;

Procedure doi_fii;
var q,tq:reper;
begin
 q:=p^.d;
 tq:=p;
 while q^.s<>nil do
  begin
   tq:=q;
   q:=q^.d;
  end;
 p^.nume:=q^.nume;
 p^.med:=q^.med;
 p^.nr:=q^.nr;
 if tq=p then tq^.d:=q^.d
         else tq^.s:=q^.d;
end;

Procedure sterg;
begin
 if (p^.s=nil) and (p^.d=nil) then nod_t
 else if (p^.s<>nil) and (p^.d=nil) then un_fiu_s
 else if (p^.s=nil) and (p^.d<>nil) then un_fiu_d
 else if (p^.s<>nil) and (p^.d<>nil) then doi_fii;
end;

Procedure stergere;
var test:boolean;
begin
 p:=rad;
 tp:=rad;
 repeat
  test:=false;
  if p^.nr=x[i] then
                 begin
                  if p=rad then tp:=nil;
                  test:=true;
                  sterg;
                 end;
  if x[i]<p^.nr then
                 begin
                  tp:=p;
                  p:=p^.s;
                 end;
  if x[i]>p^.nr then
                 begin
                  tp:=p;
                  p:=p^.d;
                 end;
 until (test) or (p=nil);
end;

Procedure listare(p:reper);
begin
 if p<>nil then
            begin
             write(p^.nume,' ');
             listare(p^.s);
             listare(p^.d);
            end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightblue);
 write('Numele : ');
 readln(c);
 write('Nr. matricol : ');
 readln(n);
 write('Media : ');
 readln(m);
 rad:=nil;
 while n<>0 do
  begin
   creare(rad);
   write('Numele : ');
   readln(c);
   write('Nr. matricol : ');
   readln(n);
   write('Media : ');
   readln(m);
  end;
 i:=0;
 cautare(rad);
 textcolor(yellow);
 write(' Inainte de stergere : ');
 listare(rad);
 sir:=i;
 i:=0;
 while i<sir do
  begin
   inc(i);
   stergere;
  end;
 writeln;
 write(' Dupa stergere : ');
 listare(rad);
 readkey;
end.