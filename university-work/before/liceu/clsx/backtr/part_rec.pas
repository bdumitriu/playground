Program de_aflare_a_partitiilor_unui_numar_recursiv;
uses crt;
var n,i:integer;
    p:array[1..1000] of integer;

Procedure scrie(i:integer);
var x:integer;
begin
 for x:=1 to i-1 do
  write(p[x],'+');
 write(p[i],';');
 write(' Apasati ENTER pt. continuare.');
 readln;
end;

Function suma(i:byte):integer;
var y,s:integer;
begin
 s:=0;
 for y:=1 to i do
  s:=s+p[y];
 suma:=s;
end;

Procedure part(i:byte);
var j,z:integer;
    cont:boolean;
begin
 for j:=1 to n do
  begin
   cont:=true;
   p[i]:=j;
   for z:=1 to i-1 do
    if p[z]>p[i] then cont:=false;
   if cont then
    if suma(i)<n then
                  part(i+1)
                 else
                  if suma(i)=n then
                                if i>1 then
                                        scrie(i);
  end;
end;

begin
 clrscr;
 writeln;
 writeln('Acesta este un program care daca ii veti introduce un numar va va');
 writeln('afisa partitiile acelui numar.');
 writeln;
 write('Numarul este ');readln(n);
 writeln('Partitiile numarului ',n,' sunt :');
 part(1);
 readkey;
end.