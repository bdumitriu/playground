Program cuvinte_pentru_invatarea_listei_dublu_inlantuite;
uses crt;
type reper=^elem;
     elem=record
           prec,urm:reper;
           inf:string;
          end;
var cap1,cap2:reper;
    c:char;

Procedure santinele;
begin
 new(cap1);
 new(cap2);
 cap1^.prec:=nil;
 cap1^.urm:=cap2;
 cap2^.prec:=cap1;
 cap2^.urm:=nil;
end;

Procedure creare;
var p,q:reper;
    n:string;
begin
 write('Cuvantul : ');readln(n);
 p:=cap1;
 while n<>'*' do
  begin
   new(q);
   q^.inf:=n;
   q^.prec:=p;
   q^.urm:=cap2;
   p^.urm:=q;
   cap2^.prec:=q;
   p:=q;
   write('Cuvantul : ');
   readln(n);
  end;
end;

Procedure inserare1;
label 1,2;
var p,q:reper;
    n:string;
begin
 write('Introduceti cuvantul in fata caruia doriti sa faceti inserarea : ');
 readln(n);
 p:=cap1^.urm;
 repeat
  if p^.inf=n then goto 1;
  p:=p^.urm;
 until p=cap2;
 writeln('Cuvantul introdus de dumneavoastra nu se afla in lista.');
 writeln('Apasati orice tasta pentru continuare.');
 readkey;
 goto 2;
 1:
 write('Introduceti cuvantul pe care doriti sa-l inserati : ');readln(n);
 new(q);
 p^.prec^.urm:=q;
 q^.prec:=p^.prec;
 q^.urm:=p;
 p^.prec:=q;
 q^.inf:=n;
 2:
end;

Procedure inserare2;
label 1,2;
var p,q:reper;
    n:string;
begin
 write('Introduceti cuvantul in dupa care doriti sa faceti inserarea : ');
 readln(n);
 p:=cap1^.urm;
 repeat
  if p^.inf=n then goto 1;
  p:=p^.urm;
 until p=cap2;
 writeln('Cuvantul introdus de dumneavoastra nu se afla in lista.');
 writeln('Apasati orice tasta pentru continuare.');
 readkey;
 goto 2;
 1:
 write('Introduceti cuvantul pe care doriti sa-l inserati : ');readln(n);
 new(q);
 p^.urm^.prec:=q;
 q^.urm:=p^.urm;
 q^.prec:=p;
 p^.urm:=q;
 q^.inf:=n;
 2:
end;

Procedure stergere;
label 1,2;
var p:reper;
    n:string;
begin
 write('Introduceti elementul pe care doriti sa-l stergeti : ');readln(n);
 p:=cap1^.urm;
 repeat
  if p^.inf=n then goto 1;
  p:=p^.urm;
 until (p=cap2) or (p=nil);
 writeln('Cuvantul introdus de dumneavoastra nu se afla in lista.');
 writeln('Apasati orice tasta pentru continuare.');
 readkey;
 goto 2;
 1:
 p^.prec^.urm:=p^.urm;
 p^.urm^.prec:=p^.prec;
 dispose(p);
 2:
end;

Procedure listare1;
var p:reper;
begin
 p:=cap1^.urm;
 if p=cap2 then writeln('Lista e vida.')
           else
            repeat
             write(p^.inf,' ');
             p:=p^.urm;
            until p=cap2;
 writeln;
 writeln('Apasati orice tasta pentru continuare.');
 readkey;
end;

Procedure listare2;
var p:reper;
begin
 p:=cap2^.prec;
 if p=cap1 then writeln('Lista e vida.')
           else
            repeat
             write(p^.inf,' ');
             p:=p^.prec;
            until p=cap1;
 writeln;
 writeln('Apasati orice tasta pentru continuare.');
 readkey;
end;

Procedure oprire;
var p:reper;
begin
 p:=cap1^.urm;
 dispose(cap1);
 p^.prec:=nil;
 if p<>cap2 then
             repeat
              p:=p^.urm;
              dispose(p^.prec);
             until p=cap2;
 dispose(cap2);
end;

begin
 santinele;
 repeat
  clrscr;
  writeln;
  textcolor(lightgreen);
  writeln('Acestea sunt comenzile disponibile : ');
  writeln('c  = crearea listei (se poate executa doar o singura data).');
  writeln('i  = inserarea unui cuvant inaintea altuia care va fi introdus');
  writeln('     de dumneavoastra.');
  writeln('j  = inserarea unui cuvant dupa un altul care va fi introdus');
  writeln('     de dumneavoastra.');
  writeln('s  = stergerea unui element introdus de dumneavoastra.');
  writeln('l  = listarea listei de la cap la coada.');
  writeln('m  = listarea listei de la coada la cap.');
  writeln('o  = oprirea programului.');
  writeln;
  textcolor(lightblue);
  write('Comanda dumneavoastra : ');readln(c);
  writeln;
  textcolor(yellow);
  case c of
   'c' : creare;
   'i' : inserare1;
   'j' : inserare2;
   's' : stergere;
   'l' : listare1;
   'm' : listare2;
   'o' : oprire;
  end;
 until c='o';
 textcolor(lightgray);
end.