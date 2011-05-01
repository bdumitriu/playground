Program de_aflare_a_primelor_n_numere_prime;
uses crt;
label 1;
var i,n,y:integer;

function testare(m:integer):boolean;
var x:boolean;
    j:integer;
 begin
 x:=true;
 for j:=2 to m div 2 do
  if m mod j=0 then x:=false;
 testare:=x;
 end;

begin
 clrscr;
 writeln('Acesta este un program care daca ii veti introduce un numar "n"');
 writeln('va va afisa primele n numere prime ');
 write('Introduceti numarul n : ');readln(n);
 write('Primele ',n,' numere prime sunt : ');
 y:=0;
 for i:=2 to maxint do
  begin
   if testare(i)=true then
                       begin
                        if y<n-1 then write(i,',')
                                 else writeln(i,'.');
                        y:=y+1;
                       end;
   if y=n then goto 1;
  end;
 1:writeln('Apasati orice tasta pentru continuare.');
 readkey;
end.