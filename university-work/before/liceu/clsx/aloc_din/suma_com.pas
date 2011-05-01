Program suma_diferenta_numere_complexe;
uses crt;
type nr_com=record
             re,im:integer;
            end;
     reper=^nr_com;
var p:array[1..50] of reper;
    i,n,re,im:integer;
begin
 clrscr;
 write('Numarul de numere : ');readln(n);
 for i:=1 to n do
  new(p[i]);
 for i:=1 to n do
  begin
   write('Partea reala ',i,' : ');readln(p[i]^.re);
   write('Partea imaginara ',i,' : ');readln(p[i]^.im);
  end;
 re:=0;im:=0;
 for i:=1 to n do
  begin
   re:=re+p[i]^.re;
   im:=im+p[i]^.im;
  end;
 write('Suma este ',re,'+',im,'i');
 for i:=1 to n do
  dispose(p[i]);
 readkey;
end.