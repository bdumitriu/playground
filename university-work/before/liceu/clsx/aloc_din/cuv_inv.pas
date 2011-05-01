Program inversare_cuvinte;
uses crt;
type reper=^elem;
     elem=record
           cuv:string;
           leg:reper;
          end;
var p,q,varf:reper;
    c:string;
    i:integer;

begin
 clrscr;
 writeln;
 i:=1;
 varf:=nil;
 repeat
  write('Cuvantul ',i,' : ');readln(c);
  i:=i+1;
  if c<>'*' then
             begin
              new(p);
              p^.cuv:=c;
              p^.leg:=varf;
              varf:=p;
             end;
 until c='*';
 if i=2 then write('Nu sunt cuvinte.')
        else
         begin
          q:=varf;
          repeat
           writeln(q^.cuv);
           q:=q^.leg
          until q=nil;
         end;
 q:=varf;
 repeat
  varf:=q^.leg;
  dispose(q);
  q:=varf;
 until q=nil;
 readkey;
end.
