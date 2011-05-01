Program OJ_97;
uses crt;
type sir=array[1..12] of 28..31;
const lu:sir=(31,28,31,30,31,30,31,31,30,31,30,31);
var z:1..31;
    l:1..12;
    a,i,nr:longint;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce o data (z,l,a)');
 writeln('va va spune in ce zi a saptamanii va pica sau a cazut aceasta data.');
 writeln;
 textcolor(lightblue);
 write('Anul : ');readln(a);
 repeat
  write('Luna : ');
  readln(l);
 until (l>0) and (l<13);
 if l<>2 then
  repeat
   write('Ziua : ');
   readln(z);
  until (z>0) and (z<=lu[l]);
 if (l=2) and (a mod 4 = 2) then
  repeat
   write('Ziua : ');
   readln(z);
  until (z>0) and (z<30);
 if (l=2) and (a mod 4 <> 2) then
  repeat
   write('Ziua : ');
   readln(z);
  until (z>0) and (z<29);
 writeln;
 textcolor(yellow);
 if a=1900 then
  begin
   nr:=0;
   for i:=1 to l-1 do
    nr:=nr+lu[i];
   nr:=nr+z;
   case nr mod 7 of
    1:writeln('Data de ',z,'.',l,'.',a,' a fost luni.    ');
    2:writeln('Data de ',z,'.',l,'.',a,' a fost marti.   ');
    3:writeln('Data de ',z,'.',l,'.',a,' a fost miercuri.');
    4:writeln('Data de ',z,'.',l,'.',a,' a fost joi.     ');
    5:writeln('Data de ',z,'.',l,'.',a,' a fost vineri.  ');
    6:writeln('Data de ',z,'.',l,'.',a,' a fost sambata. ');
    0:writeln('Data de ',z,'.',l,'.',a,' a fost duminica.');
   end;
  end;
 if a>1900 then
  begin
   nr:=0;
   nr:=nr+(a-1900)*365;
   for i:=1900 to a-1 do
    if i mod 4 = 2 then nr:=nr+1;
   if (a mod 4 = 2) and (l>2) then nr:=nr+1;
   for i:=1 to l-1 do
    nr:=nr+lu[i];
   nr:=nr+z;
   case nr mod 7 of
    1:writeln('Data de ',z,'.',l,'.',a,' a fost luni.    ');
    2:writeln('Data de ',z,'.',l,'.',a,' a fost marti.   ');
    3:writeln('Data de ',z,'.',l,'.',a,' a fost miercuri.');
    4:writeln('Data de ',z,'.',l,'.',a,' a fost joi.     ');
    5:writeln('Data de ',z,'.',l,'.',a,' a fost vineri.  ');
    6:writeln('Data de ',z,'.',l,'.',a,' a fost sambata. ');
    0:writeln('Data de ',z,'.',l,'.',a,' a fost duminica.');
   end;
  end;
 if a<1900 then
  begin
   nr:=0;
   nr:=nr+(1900-a-1)*365;
   for i:=a+1 to 1900 do
    if i mod 4 = 2 then nr:=nr+1;
   if (a mod 4 = 2) and (l<=2) then nr:=nr+1;
   for i:=12 downto l+1 do
    nr:=nr+lu[i];
   nr:=nr+lu[l]-z;
   case nr mod 7 of
    6:writeln('Data de ',z,'.',l,'.',a,' a fost luni.    ');
    5:writeln('Data de ',z,'.',l,'.',a,' a fost marti.   ');
    4:writeln('Data de ',z,'.',l,'.',a,' a fost miercuri.');
    3:writeln('Data de ',z,'.',l,'.',a,' a fost joi.     ');
    2:writeln('Data de ',z,'.',l,'.',a,' a fost vineri.  ');
    1:writeln('Data de ',z,'.',l,'.',a,' a fost sambata. ');
    0:writeln('Data de ',z,'.',l,'.',a,' a fost duminica.');
   end;
  end;
 readkey;
end.
