Program turnul_de_cuburi_de_inaltime_maxima;
uses crt;
type cub=record
         lat:integer;
         cul:string;
         end;
var c:array[1..100] of cub;
    n,i,z,max,contor_max:integer;
    s,sol:array[1..100] of integer;

Function ok(i:byte):boolean;
var x:byte;
begin
 ok:=true;
 for x:=1 to i-1 do
  if s[x]=s[i] then ok:=false;
 if i>1 then
  if (c[s[i-1]].cul=c[s[i]].cul) or (c[s[i-1]].lat<c[s[i]].lat) then ok:=false;
end;

Function nu_mai_sunt(i:byte):boolean;
var x,y:byte;
begin
 nu_mai_sunt:=true;
 for x:=1 to n do
  begin
   for y:=1 to i do
    if x<>s[y] then
   if (c[x].cul<>c[s[i]].cul) and (c[x].lat<=c[s[i]].lat) then
                                                           nu_mai_sunt:=false;
  end;
end;

Function inaltime(i:byte):integer;
var x:byte;
    aux:integer;
begin
 aux:=0;
 for x:=1 to i do
  aux:=aux+c[s[x]].lat;
 inaltime:=aux;
end;

Procedure atrib(i:byte);
var x:byte;
begin
 for x:=1 to i do
  sol[x]:=s[x];
 max:=inaltime(i);
 contor_max:=i;
end;

Procedure turn(i:byte);
var j:byte;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if ok(i) then
             begin
              if nu_mai_sunt(i) then if max < inaltime(i) then atrib(i);
              if nu_mai_sunt(i)=false then turn(i+1);
             end;
  end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce pentru n cuburi');
 writeln('culoarea si lungimea laturii va va afisa turnul maxim ce se poate');
 writeln('construi astfel incat culorile cuburilor alaturate sa fie diferite');
 writeln('si fara a pune un cub cu lungimea laturii mai mare decat cel de ');
 writeln('dedesubtul lui.');
 writeln;
 textcolor(lightblue);
 write('Numarul de cuburi : ');readln(n);
 for i:=1 to n do
  with c[i] do
   begin
    write('Culoarea cubului ',i,' este : ');readln(cul);
    write('Lungimea laturii cubului ',i,' este : ');readln(lat);
   end;
 max:=0;
 textcolor(yellow);
 turn(1);
 writeln;
 if sol[1]<>0 then
               begin
                write('Turnul e format din cuburile : ');
                for i:=1 to contor_max do
                 write(sol[i]:2);
                writeln;
                writeln('Inaltimea turnului este : ',max,'.');
               end
              else writeln('Nu exista solutii.');
 readkey;
end.