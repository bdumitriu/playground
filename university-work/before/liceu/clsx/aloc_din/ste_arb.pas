Program stergere;
uses crt;
label 1;
type reper=^elem;
     elem=record
          inf,fr:integer;
          s,d:reper;
          end;
var rad,p,tp:reper;
    x,n:integer;
    test:boolean;

Procedure creare(var p:reper);
begin
 if p=nil then
           begin
            new(p);
            p^.inf:=n;
            p^.fr:=1;
            p^.s:=nil;
            p^.d:=nil;
           end
          else
           begin
            if n<p^.inf then creare(p^.s);
            if n>p^.inf then creare(p^.d);
            if n=p^.inf then inc(p^.fr);
           end;
end;

Procedure listare(p:reper);
begin
 if p<>nil then
            begin
             write(p^.inf,' ');
             listare(p^.s);
             listare(p^.d);
            end;
end;

Procedure nod_t;
begin
 if p<>rad then
  begin
   if tp^.d=p then tp^.d:=nil;
   if tp^.s=p then tp^.s:=nil;
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
 if p=rad then
  begin
   rad:=rad^.s;
   dispose(p);
  end;
end;

Procedure un_fiu_d;
begin
 if p<>rad then
  begin
   if tp^.s=p then tp^.s:=p^.d;
   if tp^.d=p then tp^.d:=p^.d;
   dispose(p);
  end;
 if p=rad then
  begin
   rad:=rad^.d;
   dispose(p);
  end;
end;

Procedure doi_fii;
var q,tq:reper;
begin
 q:=p^.d;
 tq:=p;
 while q^.s<>nil do
  begin
   tq:=q;
   q:=q^.s;
  end;
 p^.inf:=q^.inf;
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

begin
 clrscr;
 writeln;
 write('Elementul : ');
 readln(n);
 while n<>0 do
  begin
   creare(rad);
   write('Elementul : ');
   readln(n);
  end;
 writeln;
 write('Arborele : ');
 listare(rad);
 writeln;
 write('Ce nod doriti sa stergeti ? ');readln(x);
 p:=rad;
 tp:=rad;
 repeat
  test:=false;
  if x=p^.inf then test:=true;
  if x<p^.inf then
               begin
                tp:=p;
                p:=p^.s;
                goto 1;
               end;
  if x>p^.inf then
               begin
                tp:=p;
                p:=p^.d;
               end;
  1:
 until (test) or (p=nil);
 if p=rad then tp:=nil;
 sterg;
 write('Arborele dupa stergere : ');
 listare(rad);
 readkey;
end.