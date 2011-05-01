Program bila_pe_munti;
uses crt;
type sir=array[1..4] of shortint;
const x:sir=(0,1,0,-1);
      y:sir=(-1,0,1,0);
var bi,bj,i,j,nr,m,n:integer;
    s:array[1..50,1..50] of integer;
    mat:array[1..50,1..50] of integer;

Procedure scrie;
var a,b:byte;
begin
 nr:=nr+1;
 writeln('Solutia ',nr,' : ');
 for a:=1 to m do
  begin
   for b:=1 to n do
    write(s[a,b]:3);
   writeln;
  end;
 readln;
end;

Function mai_sunt(f,g:integer):boolean;
var k:1..4;
begin
 mai_sunt:=false;
 for k:=1 to 4 do
  if mat[x[k],y[k]]>0 then if mat[x[k],y[k]]<mat[f,g] then mai_sunt:=true;
end;

Procedure traseu(i,j,pas:byte);
var k:1..4;
    ii,jj:shortint;
begin
 for k:=1 to 4 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
   if (ii in [1..m]) and (jj in [1..n]) then
    if mat[ii,jj]<mat[i,j] then
                            begin
                             s[ii,jj]:=pas;
                             if (ii in [1,m]) or (jj in [1,n]) then
                                                                 begin
                                                                  scrie;
                               if mai_sunt(ii,jj) then traseu(ii,jj,pas+1);
                                                                 end
                                                               else
                                                      traseu(ii,jj,pas+1);
                             s[ii,jj]:=0;
                            end;
  end;
end;


begin
 clrscr;
 writeln;
 textcolor(lightgreen);
 writeln('Acesta este un program care daca ii veti introduce un teren sub');
 writeln('forma de matrice, elementele matricii reprezentand cotele fie-');
 writeln('carui varf al acelui teren si coordonatele unei mingi va va afisa');
 writeln('toate drumurile prin care bila poate iesi de pe teren (adica sa ');
 writeln('ajunga pe o margine a matricii) stiind ca bila nu se poate deplasa');
 writeln('decat de pe varfuri mai inalte pe varfuri mai joase.');
 writeln;
 textcolor(lightblue);
 write('Numarul de linii : ');readln(m);
 write('Numarul de coloane : ');readln(n);
 for i;=1 to m do
  for j:=1 to n do
   begin
    write('Elementul ',i,',',j,' : ');readln(mat[i,j]);
   end;
 write('Pozitia initiala a bilei : ');readln(bi,bj);
 s[bi,bj]:=1;
 traseu(bi,bj,2);
 if nr=0 then writeln('Nu exista solutii.');
 readkey;
end.