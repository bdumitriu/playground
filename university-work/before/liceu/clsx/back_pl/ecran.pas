Program ecran;
uses crt;
var mat:array [1..25,1..80] of char;
    mat2:array [1..25,1..80] of integer;
    a,b,m,n,i,j,k,poz,pri,prj,uli,ulj,z:integer;
    com:string;
    linie:array [1..50] of char;

Procedure scrie;
var x,y:integer;
begin
 for x:=1 to m do
  begin
   for y:=1 to n do
    write(mat[x,y]:2);
   writeln;
  end;
 readln;
end;

Procedure ecr(i,ii,jj:integer);
var x,y:integer;
begin
 case linie[i] of
  'E','e':begin uli:=ii; ulj:=jj+1;mat[uli,ulj]:='*';end;
  'V','v':begin uli:=ii; ulj:=jj-1;mat[uli,ulj]:='*';end;
  'N','n':begin uli:=ii-1; ulj:=jj;mat[uli,ulj]:='*';end;
  'S','s':begin uli:=ii+1; ulj:=jj;mat[uli,ulj]:='*';end;
  end;
  if i mod k = 0 then
                  begin
                   mat[pri,prj]:='o';
                   z:=z+1;
                   for x:=1 to m do
                    for y:=1 to n do
                     if mat2[x,y]=z then
                                     begin
                                      pri:=x;
                                      prj:=y;
                                     end;
                  end;
  if (uli<1) or (uli>m) or (ulj<1) or (ulj>n) then writeln('Iese din ecran.');
  if mat2[uli,ulj]>0 then write('A ajuns pe pozitie ocupata.');
  if not(uli<1) and not(uli>m) and not(ulj<1) and not(ulj>n) and
     not(mat2[uli,ulj]>0) then
                           begin
                            scrie;
                            mat2[uli,ulj]:=poz+i;
                            ecr(i+1,uli,ulj);
                           end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de linii : ');readln(m);
 write('Numarul de coloane : ');readln(n);
 write('Numarul de pozitii : ');readln(poz);
 for i:=1 to m do
  for j:=1 to n do
   begin
    mat[i,j]:='o';
    mat2[i,j]:=0;
   end;
 for i:=1 to poz do
  begin
   write('Pozitia ',i,' : ');
   readln(a,b);
   if i=1 then
           begin
            pri:=a;
            prj:=b;
           end;
   if i=poz then
             begin
              uli:=a;
              ulj:=b;
             end;
   mat[a,b]:='*';mat2[a,b]:=i;
  end;
 write('Introduceti k : ');readln(k);
 write('Introduceti sirul de comenzi : ');readln(com);
 for i:=1 to length(com) do
  linie[i]:=com[i];
 z:=1;
 for i:=1 to m do
  begin
   for j:=1 to n do
    write(mat[i,j]:2);
   writeln;
  end;
 readln;
 ecr(1,uli,ulj);
 readkey;
end.