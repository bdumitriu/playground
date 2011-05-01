Program n_expresii_poloneze;
uses crt;
type reper=^elem;
     elem=record
           s,d:reper;
           inf:char;
          end;
var e1,e2:array[1..20] of char;
    p1,p2:array[1..20] of integer;
    s:string;
    ind,i,j,n,l:integer;
    x,c:real;
    rad:reper;

Procedure creare(li,ls:integer;var p:reper);
var i,poz,min:integer;
begin
 min:=p2[li];
 poz:=li;
 for i:=li to ls do
  if p2[i]<min then
                begin
                 min:=p2[i];
                 poz:=i;
                end;
 new(p);
 p^.inf:=e2[poz];
 if li=ls then
           begin
            p^.s:=nil;
            p^.d:=nil;
           end
          else
           begin
            creare(li,poz-1,p^.s);
            creare(poz+1,ls,p^.d);
           end;
end;

Procedure listare(p:reper);
begin
 if p<>nil then
  begin
   listare(p^.s);
   write(p^.inf);
   listare(p^.d);
  end;
end;

Function val(p:reper):real;
var m:integer;
begin
 if p<>nil then
            case p^.inf of
              '+':begin
                   val:=val(p^.s)+val(p^.d);
                   {c:=val(p^.s)+val(p^.d);}
                  end;
              '-':begin
                   val:=val(p^.s)-val(p^.d);
{                   c:=val(p^.s)-val(p^.d);}
                  end;
              '*':begin
                   val:=val(p^.s)*val(p^.d);
 {                  c:=val(p^.s)*val(p^.d);}
                  end;
              '/':begin
                   val:=val(p^.s)/val(p^.d);
{                   c:=val(p^.s)/val(p^.d);  }
                  end;
            else
             begin
              write(p^.inf,' = ');
              readln(m);
              val:=m;
             end;
            end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce un numar n ');
 writeln('va va cere sa introduceti n expresii pe care le va evalua pe');
 writeln('pe rand.');
 writeln;
 textcolor(lightblue);
 write('Numarul de expresii : ');
 readln(n);
 ind:=0;
 while ind<n do
  begin
   inc(ind);
   write(' Expresia ', ind ,' : ');readln(s);
   for i:=1 to length(s) do
    begin
     e1[i]:=s[i];
     p1[i]:=0;
    end;
  j:=0;
  for i:=1 to length(s) do
   case e1[i] of
     '(':j:=j+10;
     ')':j:=j-10;
     '+','-':p1[i]:=1+j;
     '*','/':p1[i]:=10+j
     else p1[i]:=1000;
   end;
  j:=0;
  for i:=1 to length(s) do
   if (e1[i]<>'(') and (e1[i]<>')') then
    begin
     inc(j);
     e2[j]:=e1[i];
     p2[j]:=p1[i];
    end;
  creare(1,j,rad);
  gotoxy(2,wherey);
  write(' expresia in arbore : ');
  listare(rad);
  writeln;
  x:=val(rad);
  write('  valoarea expresiei e ',x,'.');
  writeln;
 end;
 readkey;
end.