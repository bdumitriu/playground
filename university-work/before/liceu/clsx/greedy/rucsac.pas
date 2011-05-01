Program problema_rucsacului(pg_28_in_Program_calc_si_structuri);
uses crt;
const max = 20;
type sir = array[1..max] of integer;
var v, g : sir;
    g_max, n, i, val : integer;

Procedure repartitie(g_max, n : integer; v, g : sir; var val, m : integer);
var i, j : integer;
begin

end;

begin
     clrscr;
     writeln;
     write('Greutatea maxima ce poate fi transportata : ');
     readln(g_max);
     write('Numarul de obiecte : ');
     readln(n);
     for i:= 1 to n do
      begin
           write(' greutatea obiectului ',i,' : ');
           readln(g[i]);
           write(' valoarea obiectului ',i,' : ');
           readln(v[i]);
      end;
     repartitie(g_max, n, v, g, val, m);
     for i:= 1 to
     write('
     write('Valoarea maxima transportata este de ',val,'.');
     readkey;
end.