program conversie_din_scriere_romana_in_scriere_araba;
uses crt;
var cifra:char;
    val_prec,val_urm:1..1000;          {cifra prcedenta}
                                       {cifra urmatoare}
    n:integer;                         {numarul cu cifre arabe}
    corect:boolean;
begin
writeln('Acesta este un program care daca ii veti introduce un nr. scris cu');
writeln('cifre romane vi-l va converti pe acesta in cifre arabe');
n:=0;
write('Introduceti numarul cu cifre romane(M,D,C,L,X,V,I):');
read(cifra);corect:=TRUE;
{initializarea valorii cifrei precedente}
if cifra='M'
then val_prec:=1000
else if cifra='D'
then val_prec:=500
else if cifra='C'
then val_prec:=100
else if cifra='L'
then val_prec:=50
else if cifra='X'
then val_prec:=10
else if cifra='V'
then val_prec:=5
else if cifra='I'
then val_prec:=1
else corect:=FALSE;
while corect and not eoln do
begin
read(cifra);
case cifra of
 'M':val_urm:=1000;
 'D':val_urm:=500;
 'C':val_urm:=100;
 'L':val_urm:=50;
 'X':val_urm:=10;
 'V':val_urm:=5;
 'I':val_urm:=1
 else corect:=FALSE;
end;
if corect
  then begin if val_prec<val_urm
               then n:=n-val_prec
               else n:=n+val_prec;
              val_prec:=val_urm;
       end;
end;
if corect
  then writeln('Numarul in scriere araba este ',n+val_prec)
  else writeln('Numarul nu e scris corect cu cifre romane');
while not keypressed do;
end.