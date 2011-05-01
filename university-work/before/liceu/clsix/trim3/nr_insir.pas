Program de_aflare_a_numarului_de_ordine_al_unui_numar_din_un_sir;
uses crt;
type sir=array[1..50] of integer;
var a:sir;
    i,n,c:integer;

function verificare (b:sir;m:integer):integer;
var x:integer;
begin
 x:=0;
 for i:=1 to c do
   if m=b[i] then x:=i;
 verificare:=x;
end;

begin
 clrscr;
 writeln('Acesta este un program care daca ii veti introduce un sir de "n"');
 writeln('numere si apoi un numar oarecare va verifica daca respectivul ');
 writeln('numar se afla in sir sau nu si in caz ca se afla va va indica ');
 writeln('pozitia acestuia in sir, adica numarul sau de ordine');
 write('Introduceti numarul de numere : ');readln(n);
 for i:=1 to n do
  begin
   write('a[',i,']=');readln(a[i]);
  end;
 writeln('Introduceti numarul care doriti sa verificati daca se afla in sir ');
 write('sau nu : ');readln(c);
 if verificare(a,c)=0 then writeln('Numarul introdus de dumneavoastra nu se afla in sir.')
                      else writeln('Numarul de ordine al numarului introdus de dumneavoastra este ',verificare(a,c),'.');

 writeln('Apasati orice tasta pentru a continua.');
 readkey;
end.