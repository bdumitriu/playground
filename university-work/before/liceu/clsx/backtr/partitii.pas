Program partitiile_unui_numar;
uses crt;
var a:array[1..100] of integer;
    b:array[0..100] of integer;
    i,j,n:integer;
begin
clrscr;
writeln;
writeln('Acesta este un program care daca ii veti introduce un numar va afi-');
writeln('sa partitiile numarului respectiv.(partitia unui numar=toate sumele');
writeln('posibile de numere care dau numarul respectiv; ex: daca n=4 atunci par-');
writeln('titiile sale sunt 1+1+1+1,1+1+2,1+3,2+2,4)');
writeln;
write('Introduceti numarul a carui partitii doriti sa le aflati : ');
readln(n);
i:=1;
a[i]:=0;
b[0]:=0;
repeat
 while a[i]<n-b[i-1] do
  begin
   a[i]:=a[i]+1;
   b[i]:=b[i-1]+a[i];
   if b[i]=n then
    begin
     write('n=',a[1]);
     if i>1 then
      begin
       for j:=2 to i-1 do
       write('+',a[j]);
       write('+',a[i],';');
       readln;
      end
            else
       write('.');
    end
             else
    begin
     i:=i+1;
     a[i]:=a[i-1]-1;
    end;
   end;
 i:=i-1;
until i=0;
readkey;
end.