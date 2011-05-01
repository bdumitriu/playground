Program prefix_maximal;
uses crt;
const n=3;
      m=3;
type matrice=array[1..m,1..n] of char;
     sir=array[1..4] of shortint;
     rec=record
          iii,jjj:integer;
         end;
const mat:matrice=(('M','M','A'),
                   ('I','U','M'),
                   ('A','M','A'));
      x:sir=(0,1,0,-1);
      y:sir=(-1,0,1,0);
var a:string[50];
    linie:array[1..50] of char;
    s1,fin1:array[1..50] of rec;
    s2,fin2:array[1..50] of char;
    i,j,q,l,max:integer;


Function gata(ii,jj,pas:integer):boolean;
var aux:boolean;
    k:byte;
begin
 gata:=true;
 aux:=true;
 for k:=1 to 4 do
  if mat[ii+x[k],jj+y[k]]=linie[pas+1] then
                                        begin
                                         gata:=false;
                                         aux:=false;
                                        end;
end;

Procedure prefix(i,j,pas:byte);
var k:byte;
    ii,jj:shortint;
begin
 for k:=1 to 4 do
  begin
   ii:=i+x[k];
   jj:=j+y[k];
   if (ii in [1..m]) and (jj in [1..n]) then
    if mat[ii,jj]=linie[pas] then
     begin
      s1[pas].iii:=ii;
      s1[pas].jjj:=jj;
      s2[pas]:=mat[ii,jj];
      if gata(ii,jj,pas) then if pas>max then
                                          begin
                                           fin1:=s1;
                                           fin2:=s2;
                                           max:=pas;
                                          end;
      if not gata(ii,jj,pas) then prefix(ii,jj,pas+1);
      s1[pas].iii:=0;
      s1[pas].jjj:=0;
      s2[pas]:=' ';
     end;
   end;
end;

begin
 clrscr;
 writeln;
 write('Introduceti cuvantul : ');readln(a);
 q:=length(a);
 for i:=1 to q do
  linie[i]:=a[i];
 linie[q+1]:=' ';
 max:=0;
 for i:=1 to m do
  for j:=1 to n do
   if linie[1]=mat[i,j] then
                         begin
                          s1[1].iii:=i;
                          s1[1].jjj:=j;
                          s2[1]:=mat[i,j];
                          prefix(i,j,2);
                          s1[1].iii:=0;
                          s1[1].jjj:=0;
                          s2[1]:=' ';
                         end;
 writeln;
 write('Prefixul maximal este : ');
 for i:=1 to max do
  write(fin2[i]);
 writeln;
 write('Pozitiile sunt : ');
 for i:=1 to max do
  write('(',fin1[i].iii,',',fin1[i].jjj,') ');
 readkey;
end.