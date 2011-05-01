Program suma_a_doua_numere_mai_mari_decat_maxlongint;
uses crt;
var a,b:string;
    x,y,i,k,t,max,code:integer;
    j:array[1..100] of 0..9;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce doua numere');
 writeln('formate din mai mult de 10 cifre va va realiza suma lor.');
 writeln;
 textcolor(lightblue);
 write('Primul numar : ');readln(a);
 write('Al doilea numar : ');readln(b);
 if length(a) >= length(b) then max:=length(a)
                           else max:=length(b);
 writeln;
 textcolor(yellow);
 i:=length(a);
 k:=length(b);
 t:=0;
 if length(a)>=length(b) then
                          repeat
                           val(a[i],x,code);
                           val(b[k],y,code);
                           i:=i-1;
                           k:=k-1;
                           j[i+1]:=(x+y+t) mod 10;
                           t:=(x+y+t) div 10;
                          until i=0
                         else
                          repeat
                           val(a[i],x,code);
                           val(b[k],y,code);
                           i:=i-1;
                           k:=k-1;
                           j[k+1]:=(x+y+t) mod 10;
                           t:=(x+y+t) div 10;
                          until k=0;

 write('Suma obtinuta : ');
 if t>0 then write(t);
 for i:=1 to max do write(j[i]);
 readkey;
end.