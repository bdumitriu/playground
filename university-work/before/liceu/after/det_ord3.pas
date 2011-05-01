Program de_determinare_a_determinantului_unei_matrici_de_ordinul_trei;
uses crt;
var a:array[1..3,1..3] of integer;
    i,j,det:integer;

begin
 clrscr;
 writeln;
 for i:=1 to 3 do
  for j:=1 to 3 do
   begin
    write('Elementul ',i,',',j,' : ');
    readln(a[i,j]);
   end;
 det:=a[1,1]*a[2,2]*a[3,3]+a[3,1]*a[1,2]*a[2,3]+a[1,3]*a[2,1]*a[3,2];
 det:=det-a[1,3]*a[2,2]*a[3,1]-a[1,1]*a[2,3]*a[3,2]-a[1,2]*a[2,1]*a[3,3];
 writeln('Determinantul matricii este ',det,'.');
 readkey;
end.