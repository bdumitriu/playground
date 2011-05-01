Program turnurile_din_Hanoi;
uses crt;
var a,b,c:char;
    n:byte;

Procedure Hanoi(n:byte;a,b,c:char);
begin
 if n=1 then writeln(a,'-',b)
        else
         begin
          Hanoi(n-1,a,c,b);
          writeln(a,'-',b);
          Hanoi(n-1,c,b,a);
          readkey;
         end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de discuri de pe tija A : ');readln(n);
 a:='A';
 b:='B';
 c:='C';
 Hanoi(n,a,b,c);
 readkey;
end.