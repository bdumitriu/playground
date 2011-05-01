Program Subiectul_IV;
uses crt;

type sir = array [1..100] of integer;

var a: sir;
    n, i: integer;

Function max(x, y: integer): integer;
begin
   max := y;
   if x >= y then
      max := x
end;

Procedure Interclasare(inf, sup, mij: integer; var a: sir);
var i, j, idx: integer;
    b: sir;
begin
   i := inf;
   j := mij+1;
   idx := 1;

   repeat                   {se interclaseaza elementele pana cand se}
      if a[i] <= a[j] then  {epuizeaza unul dintre siruri}
      begin
         b[idx] := a[i];
         inc(i);
      end
      else
      begin
         b[idx] := a[j];
         inc(j);
      end;
      inc(idx);
   until (i > mij) or (j > sup);

   if i <= mij then         {se introduc in sir elemetele ramase din sirul}
      for j := i to mij do  {neepuizat}
      begin
         b[idx] := a[j];
         inc(idx);
      end
   else
      for i := j to sup do
      begin
         b[idx] := a[i];
         inc(idx);
      end;

   idx := 1;
   for i := inf to sup do
   begin
      a[i] := b[idx];
      inc(idx);
   end;
end;

Procedure Divide(inf, sup: integer; var a: sir);
var mij: integer;
begin
   if inf < sup then
   begin
      mij := (sup+inf-1) div 2;
      Divide(inf, mij, a);
      Divide(mij+1, sup, a);
      Interclasare(inf, sup, mij, a);
   end;
end;

begin
   clrscr;
   writeln;
   write(' Numarul de elemente din sir: ');
   readln(n);
   for i := 1 to n do
   begin
      write(' a[', i, '] = ');
      readln(a[i]);
   end;
   Divide(1, n, a);
   write(' Sirul ordonat este: ');
   for i := 1 to n do
      write(a[i], ' ');

   readkey;
end.