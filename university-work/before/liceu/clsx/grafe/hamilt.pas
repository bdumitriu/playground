Program care_determina_daca_un_graf_e_hamiltonian_sau_nu;
uses crt;
label 1;
var n,i,j,a,nr:integer;
    mat:array[1..50,1..50] of integer;
    s:array[1..50] of integer;

Function ok(i:byte):boolean;
var x:byte;
begin
 ok:=true;
 for x:=1 to i-1 do
  if s[x]=s[i] then ok:=false;
 if mat[s[i],s[i-1]]=0 then ok:=false;
end;

Procedure hamil(i:byte);
var j:byte;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if ok(i) then
    if i<n then hamil(i+1)
           else if mat[s[n],1]=1 then nr:=1;
  end;
end;

begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care determina daca un graf e hamiltonian ');
 writeln('sau nu.');
 textcolor(lightblue);
 writeln;
 write('Numarul de varfuri ale grafului : ');
 readln(n);
 for i:=1 to n do
  for j:=1 to n do
   begin
    write('Matrice de adiacenta [',i,',',j,'] : ');
    readln(mat[i,j]);
   end;
 nr:=0;
 textcolor(yellow);
 writeln;
 s[1]:=1;
 if n<3 then
         begin
          writeln('Graful nu e hamiltonian.');
          goto 1;
         end
        else hamil(2);
 if nr=0 then writeln('Graful nu e hamiltonian.')
         else writeln('Graful e hamiltonian.');
 1:textcolor(lightgray);
 readkey;
end.