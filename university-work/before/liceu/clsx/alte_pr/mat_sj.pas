Program matrice_parcursa_sus_jos;
uses crt;
var a,i,j,m,n:shortint;
    mat:array[1..10,1..10] of integer;
    s:array[1..100] of integer;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce o matrice pa-');
 writeln('tratica va va afisa matricea parcursa de sus-jos, jos-sus, etc.');
 writeln;
 textcolor(lightblue);
 write('Introduceti numarul de linni si coloane ale matricii : ');readln(n);
 for i:=1 to n do
  for j:=1 to n do
   begin
    write('Elementul ',i,',',j,' : ');
    readln(mat[i,j]);
   end;
 writeln;
 textcolor(yellow);
 m:=0;
 j:=1;
 repeat
  if j mod 2 = 1 then
                  begin
                   for i:=1 to n do
                    begin
                     m:=m+1;
                     s[m]:=mat[i,j];
                    end;
                   j:=j+1;
                  end
                  else
                   begin
                    for i:=n downto 1 do
                     begin
                      m:=m+1;
                      s[m]:=mat[i,j];
                     end;
                    j:=j+1;
                   end;
 until m=n*n;
 for i:=1 to n*n do
  write(s[i],' ');
 readkey;
end.