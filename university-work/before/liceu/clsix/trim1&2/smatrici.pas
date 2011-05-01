Program adunare_a_doua_matrici;
uses crt;
const m=4;
      n=3;
type matrice=array[1..m,1..n] of integer;
var a,b,c:matrice;
    i,j:integer;
begin
for i:=1 to m do begin
                 for j:=1 to n do begin
                                  write('a[',i,',',j,']=');read(a[i,j]);
                                  write('b[',i,',',j,']=');read(b[i,j]);
                                  end;
                 end;
for i:=1 to m do begin
                 for j:=1 to n do begin
                                  c[i,j]:=a[i,j]+b[i,j];
                                  end;
                 end;
for i:=1 to m do begin
                 for j:=1 to n do begin
                                  writeln('c[',i,',',j,']=',c[i,j]);
                                  end;
                 end;
while not keypressed do;
readln;
end.