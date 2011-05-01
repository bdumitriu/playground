Program submultime_la_care_daca_ii_insumam_elementele_ne_da_un_nr_divizibil;
uses crt;
var m,ind:array [1..100] of integer;
    i,n,rest,li,lf,s:integer;

begin
 clrscr;
 writeln;
 write('Numarul de elemente a multimii : ');readln(n);
 for i:=1 to n do
  begin
   write('Elementul ',i,' : ');readln(m[i]);
  end;
 s:=0;
 for i:=0 to n-1 do
  ind[i]:=0;
 for i:=1 to n do
  begin
   s:=s+m[i];
   rest:=s mod n;
   if rest=0 then
              begin
               li:=1;
               lf:=i;
               i:=n;
              end
             else if ind[rest]=0 then ind[rest]:=i
                                 else
                                  begin
                                   li:=ind[rest]+1;
                                   lf:=i;
                                  end;
  end;
 for i:=li to lf do
  write(m[i]:3);
 readkey;
end.