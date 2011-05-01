Program arbore_partial_de_cost_minim(pg_6_in_Bazele_stiintei_sist_si_calc);
uses crt;
const max = 5; {numarul maxim al nodurilor grafului}
      max1 = max-1;
type matrice = array[1..max,1..max] of integer;
     vector = array[1..max] of integer;
     tablou = array[1..max1,1..2] of integer;
var n, m, i, j, a, b, d : integer;
    MUCHII : tablou;
    COST : integer;
    C : matrice;

Procedure prim(n : integer; C : matrice; var MUCHII : tablou; var COST : integer);
{n = nr. nodurilor grafului, C = matricea de cost atasata grafului
 MUCHII = tabloul ce contine extremitatile muchiilor, COST = costul arborelui
 construit}
var V : vector;
    i, j, l : integer;
    minim : integer;
begin
     COST:= 0;
     V[1]:= 0;
     for i:= 2 to n do
         V[i]:= 1;
     for i:= 1 to n-1 do
      begin
           minim:= maxint;
           j:= 0;
           for l:= 1 to n do
               if (V[l] <> 0) and (C[l,V[l]] < minim)
                  then
                   begin
                        j:= l;
                        minim:= C[l,V[l]];
                   end;
           MUCHII[i,1]:= V[j];
           MUCHII[i,2]:= j;
           COST:= COST+C[j,V[j]];
           V[j]:= 0;
           for l:= 1 to n do
               if (V[l] <> 0) and (C[l,V[l]] > C[l,j])
                  then
                      V[l]:= j;
      end;
end;

begin
     textbackground(black);
     textcolor(lightgray);
     clrscr;
     writeln;
     write('Numarul de noduri al grafului : ');
     readln(n);
     write('Numarul de muchii : ');
     readln(m);
     for i:= 1 to m do
      begin
           write('Muchia ',i,' este intre nodurile : ');
           readln(a,b);
           write('Costul muchiei dintre nodurile ',a,' si ',b,' este : ');
           readln(d);
           C[a,b]:= d;
           C[b,a]:= d;
      end;
     for i:= 1 to n do
         for j:= 1 to n do
             if C[i,j] = 0 then C[i,j]:= maxint;
     prim(n, C, MUCHII, COST);
     writeln;
     writeln('Noul graf are muchiile : ');
     for i:= 1 to n-1 do
          writeln(' Muchia ',i,' este ',MUCHII[i,1],' ',MUCHII[i,2]);
     writeln('Costul total al arborelui partial este ',COST:2,'.');
     readkey;
end.