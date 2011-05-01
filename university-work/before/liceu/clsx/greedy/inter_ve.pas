Program interclasarea_optimala_a_mai_multor_vectori(pg_5_in_Bazele_st_sist_si_calc);
uses crt;
const mmax = 100;
type elem = record
             nr_elem : integer;
             id_vect : integer;
            end;
     sir = array[1..mmax] of elem;
var m, i : integer;
    L : sir;

Procedure intercl(m : integer; L : sir);
var i, j, k, lung, poz1, poz2 : integer;
begin
     k:= m;
     j:= 0;
     while k>1 do
      begin
           j:= j+1;
           poz1:= 1;
           poz2:= 2;
           for i:= 3 to k do
               if L[i].nr_elem < L[poz1].nr_elem
                  then
                   begin
                        poz1:= i;
                   end
                  else if L[i].nr_elem < L[poz2].nr_elem
                          then poz2:= i;
           lung:= L[poz1].nr_elem + L[poz2].nr_elem;
           write(j:2,'. Se interclaseaza vectorii V[',L[poz1].id_vect,']');
           writeln(' si V[',L[poz2].id_vect,'].');
           writeln('Rezulta vectorul V[',m+j,'] de ',lung,' elemente.');
           if poz1 < poz2
              then
               begin
                    L[poz2].nr_elem:= lung;
                    L[poz2].id_vect:= m+j;
                    for i:= poz1 to k-1 do
                        L[i]:= L[i+1];
               end
              else
               begin
                    L[poz1].nr_elem:= lung;
                    L[poz1].id_vect:= m+j;
                    for i:= poz2 to k-1 do
                        L[i]:= L[i+1];
               end;
           k:= k-1;

      end;
end;

begin
     clrscr;
     writeln;
     write('Numarul de vectori : ');
     readln(m);
     for i:= 1 to m do
      begin
           write('Numarul de elemente din vectorul ',i,' : ');
           readln(L[i].nr_elem);
           L[i].id_vect:= i;
      end;
     intercl(m, L);
     readkey;
end.