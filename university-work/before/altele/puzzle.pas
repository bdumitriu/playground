Program puzzle;
uses crt;

type matrice = array[1..4, 1..4] of 1..16;

const b: matrice = (( 1, 2, 3, 4),
                    ( 5, 6, 7, 8),
                    ( 9,10,11,12),
                    (13,14,15,16));
      x: array[1..4] of -1..1 = (-1, 0, 1, 0);
      y: array[1..4] of -1..1 = (0, 1, 0, -1);

var a: matrice;
    l: array[1..16] of integer;

Procedure CitireaDatelor;
var i, j: integer;
begin
   writeln(' Introduceti configuratia initiala (cu nr. 16 pt. gol):');
   for i := 1 to 4 do
      for j := 1 to 4 do
      begin
         write('  piesa de pe pozitia ', i, ',', j, ': ');
         readln(a[i,j]);
      end;
end;

Function NuSePoateRezolva: boolean;
var i, j, k, m, n, x: integer;
begin
   for i := 1 to 4 do
      for j := 1 to 4 do
      begin
         if a[i,j] = 16 then
            x := i+j;
         l[a[i,j]] := 0;
         m := j+1;
         for k := i to 4 do
         begin
            for n := m to 4 do
               if a[k,n] < a[i,j] then
                  l[a[i,j]] := l[a[i,j]]+1;
            m := 1;
         end;
      end;
   NuSePoateRezolva := true;
   for i := 1 to 16 do
      x := x+l[i];
   if x mod 2 = 0 then
      NuSePoateRezolva := false;
end;

Procedure ScrieMatrice(x: matrice);
var i, j: integer;
begin
   for i := 1 to 4 do
   begin
      for j := 1 to 4 do
         if a[i,j] in [1..15] then
            write(a[i,j]:3)
         else
            write('':3);
      writeln;
   end;
end;

Procedure DetGol(x: matrice; var i, j: integer);
var m, n: integer;
begin
   for m := 1 to 4 do
      for n := 1 to 4 do
         if a[m,n] = 16 then
         begin
            i := m;
            j := n;
         end;
end;

Function NrPlacute(x: matrice): integer;
var i, j, y: integer;
begin
   y := 0;
   for i := 1 to 4 do
      for j := 1 to 4 do
         if b[i,j] <> x[i,j] then
            inc(y);
   NrPlacute := y;
end;

Function Nu_iGata(x: matrice): boolean;
var i, j: integer;
begin
   Nu_iGata := false;
   for i := 1 to 4 do
      for j := 1 to 4 do
         if x[i,j] <> b[i,j] then
            Nu_iGata := true;
end;

Procedure Rezolvare;
var c: matrice;
    lis: array[1..100] of matrice;
    ct: array[1..100] of CostTata;
    tst: boolean;
    gi, gj, aux, idx, k, min, minidx, i: integer;

Procedure Scrie(ind: integer);
begin
end;

begin
   idx := 0;
   tst := false;
   i := 0;
   while not tst do
   begin
      k := 0;
      DetGol(a, gi, gj);
      while (k < 4) and (tst = false) do
      begin
         c := a;
         inc(k);
         if (gi+x[k] in [1..4]) and (gj+y[k] in [1..4]) then
         begin
            aux := c[gi+x[k],gj+y[k]];
            c[gi+x[k],gj+y[k]] := 16;
            c[gi,gj] := aux;
            if Nu_iGata(c) then
            begin
               inc(idx);
               lis[idx] := c;
               ct[idx].c1 := NrPlacute(c);
               ct[idx].t := i;
               if i <> 0 then
                  ct[idx].c2 := ct[i].c2+1
               else
                  ct[idx].c2 := 1;
            end
            else
            begin
               tst := true;
               Scrie(idx);
            end;
         end;
      end;
      if tst = false then
      begin
         min := maxint;
         for k := 1 to idx do
         begin
            if ct[k].c1+ct[k].c2 < min then
            begin
               min := ct[k].c1+ct[k].c2;
               minidx := k;
            end;
         end;
         a := lis[minidx];
         i := minidx;
      end;
   end;
end;

begin
   clrscr;
   writeln;
   CitireaDatelor;
   if NuSePoateRezolva then
   begin
      writeln(' Din configuratia data nu se poate ajunge la configuratia');
      writeln(' finala in nici un mod.');
      readkey;
      exit;
   end;
   clrscr;
   ScrieMatrice(a);
   readkey;
   Rezolvare;
end.