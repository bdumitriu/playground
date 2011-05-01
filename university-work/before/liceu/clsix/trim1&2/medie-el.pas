Program media_elevilor;
uses crt;
const n=3;
type elevi=record
           nume:string[25];
           nota_i,nota_m,nota_f:integer;
           medie:real;
           end;
var g:string[25];
    i,j:integer;
    k:real;
    elev:array [1..n] of elevi;
begin
writeln('Acesta este un program care daca ii veti introduce numele a trei');
writeln('elevi,nota fiecaruia la informatica, la matematica si la fizica ');
writeln('ii va afisa pe elevi in ordinea descrescatoare a mediilor lor  ge-');
writeln('nerale si va afisa in dreptul fiecaruia media sa generala');
writeln;
for i:=1 to n do begin
write('nume[',i,']=');readln(elev[i].nume);
write('nota la informatica=');readln(elev[i].nota_i);
write('nota la matematica=');readln(elev[i].nota_m);
write('nota la fizica=');readln(elev[i].nota_f);
end;
for i:=1 to n do begin
elev[i].medie:=(elev[i].nota_i+elev[i].nota_f+elev[i].nota_m)/3;
end;
for i:=1 to n-1 do
for j:=i+1 to n do begin
if elev[i].medie<elev[j].medie then begin
                                    g:=elev[i].nume;
                                    elev[i].nume:=elev[j].nume;
                                    elev[j].nume:=g;
                                    k:=elev[i].medie;
                                    elev[i].medie:=elev[j].medie;
                                    elev[j].medie:=k;
                                    end;
                   end;
for i:=1 to n do begin
writeln(elev[i].nume        ,          elev[i].medie);
end;
while not keypressed do;
end.