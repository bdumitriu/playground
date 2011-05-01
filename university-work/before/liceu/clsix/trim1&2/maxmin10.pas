Program maxmin10;
Uses crt;
Type vect=array[1..10] of integer;
Var i,max,min:integer;
    x:vect;
Begin
Writeln('Acesta este un program care daca ii veti introduce 10 numere va va');
Writeln('afisa maximul si minimul dintre ele');
Writeln('Introduceti numerele');
For i:=1 to 10 do begin
                  write('x[',i,']=');read(x[i]);
                  end;
max:=x[1];
For i:=1 to 10 do begin
                  if max<=x[i] then max:=x[i];
                  end;
min:=x[i];
For i:=1 to 10 do begin
                  if min>=x[i] then min:=x[i];
                  end;
Writeln('Maximul dintre cele 10 numere este ',max);
Writeln('Minimul dintre cele 10 numere este ',min);
While not keypressed do;
Readln;
End.