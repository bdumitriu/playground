Program nume_in_ordine_alfabetica;
uses crt;
var i,n:integer;
    nume:array[1..20] of string[20];
    ordonare:boolean;
    aux:string[30];
begin
writeln('Acesta este un program care daca ii vati introduce un numar de nume');
writeln('vi le va afisa pe acestea in ordine alfabetica.Structura unui nume:');
writeln('nume si dupa aceea prenume');
write('Introduceti numarul de nume pe care doriti sa le introduceti:  ');
readln(n);
for i:=1 to n do begin
                 write('nume[',i,']=');readln(nume[i]);
                 end;
repeat begin
       ordonare:=true;
       for i:=1 to n-1 do begin
                          if nume[i]>nume[i+1] then
                                         begin
                                         ordonare:=false;
                                         aux:=nume[i];
                                         nume[i]:=nume[i+1];
                                         nume[i+1]:=aux;
                                         end;
                          end;
       end;
until ordonare;
writeln;
writeln('Numele in ordine alfabetica sunt');
for i:=1 to n do writeln(nume[i]);
while not keypressed do;
end.