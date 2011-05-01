Program calcul_expresie_poloneza;
uses crt;
var a,c:string;
    i,j,k:integer;
    s:array[1..50,1..2] of string;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce o expresie sub');
 writeln('forma poloneza va va afisa expresia in forma normala.');
 writeln;
 textcolor(lightblue);
 write('Expresia in forma poloneza : ');readln(a);
 k:=0;j:=0;
 for i:=1 to length(a) do
  begin
   case a[i] of
    'a'..'z' : begin
                k:=k+1;
                s[k,1]:=a[i];
                s[k,2]:='';
               end;
    '+','-','*','/' : begin
                       writeln('x',j,' = ',s[k-1,1],s[k-1,2],a[i],s[k,1],
                       s[k,2]);
                       k:=k-1;
                       s[k,1]:='x';
                       str(j,c);
                       s[k,2]:=c;
                       j:=j+1;
                      end;
   end;
  end;
 readkey;
end.