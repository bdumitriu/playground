Program MetodaLuiGauss;
uses crt;
type matrice = array[1..100, 1..100] of real;
     sir = array[1..100] of real;
var a: matrice;
    b, x: sir;
    i, j, k, n, s: integer;
    test: boolean;

Procedure CitesteDateIntrare;
begin
   writeln(' Numarul de necunoscute: ');
   write(' (implicit si de linii si coloane a matricei atasate sistemului)');
   gotoxy(26, wherey-1);
   readln(n);
   gotoxy(1, wherey+1);
   writeln(' Acum introduceti matricea atasata sisetmului:');
   for i := 1 to n do
      for j := 1 to n do
      begin
         write('  coeficientul lui x', j, ' din ecuatia a ', i, '-a: ');
         readln(a[i,j]);
      end;
   writeln(' Acum introduceti sirul termenilor liberi:');
   for i := 1 to n do
   begin
      write('  termenul liber a ecuatiei a ', i, '-a: ');
      readln(b[i]);
   end;
end;

Procedure SchimbaColoane(k, s: integer);
var aux: real;
begin
   for i := 1 to n do
   begin
      aux := a[k,i];
      a[k,i] := a[s,i];
      a[s,i] := aux;
   end;
end;

Procedure GasesteSoulutiileSistemului;
var y: real;
begin
   for s := 1 to n-1 do
   begin
      k := s;
      test := true;
      while (k <= n) and (test = true) do
         if a[k,s] = 0 then
            k := k+1
         else
            test := false;
      if test = true then
      begin
         writeln(' Sistemul nu este compatibil determinat.');
         readkey;
         halt;
      end
      else
         if k <> s then
         begin
            SchimbaColoane(k, s);
         end;
      for i := s+1 to n do
      begin
         y := a[i,s];
         for j := s to n do
            a[i,j] := a[i,j]-y*a[s,j]/a[s,s];
         b[i] := b[i]-y*b[s]/a[s,s];
      end;
   end;
end;

Procedure DeterminaNecunoscutele;
var suma: real;
begin
   for i := n downto 1 do
   begin
      suma := 0;
      for j := i+1 to n do
         suma := suma+a[i,j]*x[j];
      x[i] := (b[i]-suma)/a[i,i];
   end;
end;

{Procedure TiparesteMatrice;
begin
   for i := 1 to n do
   begin
      for j := 1 to n do
         write(a[i,j], ' ');
      writeln(b[i]);
   end;
end;}

Procedure TiparesteNecunoscutele;
begin
   for i := 1 to n do
   begin
      write(' x', i, ' = ', x[i]);
      readln;
   end;
end;

begin
   clrscr;
   writeln;

   CitesteDateIntrare;
   GasesteSoulutiileSistemului;
{   TiparesteMatrice;}
   DeterminaNecunoscutele;
   TiparesteNecunoscutele;
end.