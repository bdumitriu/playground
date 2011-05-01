Program comis_voiajor;
uses crt;
var mat1,mat2:array[1..6,1..6] of integer;
    s,sol,viz:array[1..10] of byte;
    i,j,n,l,nr_con,c,min:integer;

Function cost:integer;
var x:byte;
    aux:integer;
begin
 aux:=0;
 for x:=2 to n+1 do
  aux:=aux+mat2[s[x],s[x-1]];
 cost:=aux;
end;

Procedure atrib;
var x:byte;
begin
 for x:=1 to n+1 do
  sol[x]:=s[x];
 min:=cost;
end;

Function ok(i:byte):boolean;
var x:byte;
begin
 ok:=true;
 if (s[i]=1) and (i<n+1) then ok:=false;
 if (s[i-1]=s[i]) or (mat1[s[i-1],s[i]]=0) or (viz[s[i]]=1) then ok:=false;
end;

Procedure traseu(i:byte);
var j:byte;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if ok(i) then
    begin
     viz[s[i]]:=1;
     if s[i]=1 then if min > cost then atrib;
     if s[i]>1 then traseu(i+1);
     viz[s[i]]:=0;
    end;
  end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce un numar n de');
 writeln('orase, conectiile dintre ele si costul deplasarii pe fiecare ');
 writeln('conectie va va afisa drumul cu cost minim pe care=l poate efectua');
 writeln('un comis-voiajor care pleaca din orasul 1 si, vizitand toate cele-');
 writeln('lalte orase cate o singura data trebuie sa se intoarca de unde a ');
 writeln('plecat.');
 writeln;
 textcolor(lightblue);
 write('Introduceti numarul de orase : ');readln(n);
 write('Introduceti numarul total de conectii intre orase : ');readln(nr_con);
 for i:=1 to n do
  for j:=1 to n do
   mat1[i,j]:=0;
 for i:=1 to n do
  viz[i]:=0;
 min:=0;
 for l:=1 to nr_con do
  begin
   write('Conectia ',l,' este intre orasele : ');
   readln(i,j);
   write('Costul deplasarii intre orasele ',i,' si ',j,' este : ');
   readln(c);
   mat1[i,j]:=1;mat1[j,i]:=1;
   mat2[i,j]:=c;mat2[j,i]:=c;
   min:=min+c;
  end;
 s[1]:=1;
 traseu(2);
 textcolor(yellow);
 writeln('Comis-voiajorul va vizita orasele in ordinea urmatoare : ');
 for i:=1 to n+1 do
  write(sol[i]:3);
 writeln;
 writeln('Costul deplasarii este ',min,'.');
 readkey;
end.