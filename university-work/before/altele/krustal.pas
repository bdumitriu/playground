Program Arbore_Partial_De_Cost_Minim(APM);
uses crt;
type muchie=record
             x,y,c:integer;
            end;
var i,j,k,m,n,min:integer;
    muc:array[1..50] of muchie;
    l:array[1..50] of integer;
    aux:muchie;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce un graf oare-');
 writeln('care cu n varfuri si m muchii, fiecarei muchii fiindu-i atasat');
 writeln('un cost va va afisa drumul minim pentru a trece prin toate varfu-');
 writeln('rile.');
 writeln;
 textcolor(lightblue);
 write('Numarul de muchii : ');readln(m);
 write('Numarul de varfuri : ');readln(n);
 for i:=1 to m do
  begin
   write('Extremitatea 1 a muchiei ',i,' : ');readln(muc[i].x);
   write('Extremitatea 2 a muchiei ',i,' : ');readln(muc[i].y);
   write('Costul muchiei ',i,' : ');readln(muc[i].c);
  end;
 for i:=1 to n do l[i]:=i;
 repeat
  k:=0;
  for i:=1 to m-1 do
   if muc[i].c>muc[i+1].c then
    begin
     aux:=muc[i];
     muc[i]:=muc[i+1];
     muc[i+1]:=aux;
     k:=1;
    end;
 until k=0;
 textcolor(lightblue);
 writeln('Acesta este drumul pe care se va parcurge graful : ');
 min:=0;
 k:=0;
 i:=1;
 while k<n-1 do
  begin
   if l[muc[i].x]<>l[muc[i].y] then
                                begin
                                 k:=k+1;
                                 min:=min+muc[i].c;
                                 write('[',muc[i].x,',',muc[i].y,'] ');
                                 for j:=1 to n do
                                  if l[j]=l[muc[i].y] then l[j]:=l[muc[i].x];
                                end;
   i:=i+1;
  end;
 writeln;
 writeln('Costul total al drumului este : ',min,'.');
 readkey;
end.