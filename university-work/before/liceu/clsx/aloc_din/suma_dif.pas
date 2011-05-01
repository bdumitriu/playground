Program suma_diferenta_numere_complexe;
uses crt;
type nr_com=record
             re,im:integer;
            end;
     reper=^nr_com;
var p,q:reper;
    c:char;
begin
 clrscr;
 new(p);
 new(q);
 write('Partea reala 1 : ');readln(p^.re);
 write('Partea imaginara 1 : ');readln(p^.im);
 write('Partea reala 2 : ');readln(q^.re);
 write('Partea imaginara 2 : ');readln(q^.im);
 write('Ce doriti, suma sau diferenta ? dati s/d : ');readln(c);
 if c='s' then write('Suma este ',p^.re+q^.re,'+',p^.im+q^.im,'i');
 if c='d' then write('Diferenta este ',p^.re-q^.re,'+',p^.im-q^.im,'i');
 dispose(p);
 dispose(q);
 readkey;
end.