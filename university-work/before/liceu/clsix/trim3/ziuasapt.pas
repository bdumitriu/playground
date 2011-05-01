Program De_aflare_ce_zi_a_fost_data_de;
uses crt;
label 100;
label 200;
const n=7;
var ziua:1..31;
    luna:1..12;
    anul,an_bisect,i,q,x,y:integer;
    zilele:longint;
    zi:string;
begin
writeln('Intorodceti data');
write('ziua[zz]:');readln(ziua);
write('luna[ll]:');readln(luna);
write('anul[aaaa]:');readln(anul);
if anul<1990 then
begin
q:=2;
an_bisect:=0;
for i:=1990 downto anul do begin
if 1990-q=anul+1 then an_bisect:=an_bisect+1
               else goto 100;
q:=q+4;
end;
100:zilele:=365*(1990-anul-1)+an_bisect;
q:=2;
for i:=1990 downto anul do begin
if 1990-q=anul then goto 200;
q:=q+4;
end;
200:case luna of
              1:zilele:=zilele+335;
              2:zilele:=zilele+306;
              3:zilele:=zilele+275;
              4:zilele:=zilele+245;
              5:zilele:=zilele+214;
              6:zilele:=zilele+184;
              7:zilele:=zilele+153;
              8:zilele:=zilele+122;
              9:zilele:=zilele+92;
              10:zilele:=zilele+61;
              11:zilele:=zilele+31
              else zilele:=zilele;
              end;
case luna of
          1,3,5,7,8,10,12:zilele:=zilele+(31-ziua+1);
          2:zilele:=zilele+(29-ziua+1);
          else zilele:=zilele+(30-ziua+1);
          end;
end;
x:=zilele div n;
y:=zilele-x*n;
case y of
       0:zi:='marti';
       6:zi:='miercuri';
       5:zi:='joi';
       4:zi:='vineri';
       3:zi:='sambata';
       2:zi:='duminica';
       1:zi:='luni';
       end;
writeln('Ziua de ',ziua,'.',luna,'.',anul,' a fost ',zi);
while not keypressed do;
end.
