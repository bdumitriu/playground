Program prosum10;
uses crt;
type vect=array[1..10] of integer;
var S,P,i:integer;
    x:vect;
begin
Writeln('Acesta este un program care daca ii veti introduce 10 numere');
Writeln('va va afisa suma dintre primul,al treilea,al cincelea,al saptelea');
Writeln('si al noualea si produsul dintre al doilea,al patrulea,al saselea,');
Writeln('al optulea si al zecelea.Daca unul din numerele care se vor');
Writeln('inmulti este 0 acesta nu va fi inclus in calcul');
Writeln('Introduceti cele 10 numere');
S:=0;
P:=1;
for i:=1 to 10 do begin
                  Write('x[i]=');read(x[i]);
                  end;
for i:=1 to 10 do begin
                  if x[i]<>0 then begin
                                  if i/2=(i div 2) then P:=P*x[i]
                                                   else S:=S+x[i];
                                  end
                             else begin
                                  if i/2=(i div 2) then begin
                                                        if x[i]<>0 then P:=P*x[i]
                                                                   else P:=P;
                                                        end
                                                   else S:=S+x[i];
                                  end;
                  end;
Writeln('Suma este',S);
Writeln('Produsul este',P);
while not keypressed do;
Readln;
end.