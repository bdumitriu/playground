Program olimpiada_problema_1;
uses crt;
var f,g:text;
    s:array[1..50] of string;
    mat:array[1..20,1..20] of 0..1;
    sol:array[1..20] of integer;
    str:string[41];
    str2:string[20];
    n,i,j,k,a,b:integer; {n-numarul de echipe}
    mec:array[1..50] of record
                   ec1,ec2:string[20]; {ec1-cea castigatoare,ec2-cea invinsa}
                        end;
Procedure scrie;
var x:integer;
begin
 for x:=1 to n do
  writeln(g,s[sol[x]]);
 close(g);
 halt;
end;

Function ok2(i:byte):boolean;
var x:integer;
begin
 ok2:=true;
 if (i>1) and (mat[sol[i-1],sol[i]]=0) then ok2:=false;
 for x:=1 to i-1 do
  if sol[x]=sol[i] then ok2:=false;
end;

Procedure aranjare(i:byte);
var j:integer;
begin
 for j:=1 to n do
  begin
   sol[i]:=j;
   if ok2(i) then
    begin
     if i>1 then mat[sol[i-1],sol[i]]:=0;
     if i=n then scrie
            else aranjare(i+1);
     if i>1 then mat[sol[i-1],sol[i]]:=1;
    end;
  end;
end;

Function ok(str:string;j:integer):boolean;
var x:integer;
begin
 ok:=true;
 for x:=1 to j do
  if s[x]=str then ok:=false;
end;

Function corect(n,k:integer):boolean;
var x,y,z:integer;
begin
 y:=1;
 for x:=1 to n do
  y:=y*x;
 z:=1;
 for x:=1 to n-2 do
  z:=z*x;
 y:=y div z;
 y:=y div 2;
 if y=k then corect:=true
        else corect:=false;
end;

begin
 clrscr;
 writeln;
 write('Introduceti numele fisierului cu datele de intrare : ');
 readln(str);
 assign(f,str);
 writeln('Introducei numele fisierului in care trebuie introduse datele de ');
 write('iesire : ');
 read(str);
 writeln;
 assign(g,str);
 rewrite(g);
 reset(f);
 readln(f,n);
 k:=0;
 repeat
  k:=k+1;
  readln(f,str);
  for i:=1 to length(str) do
   if str[i]=' ' then
                  begin
                   str2:='';
                   for j:=1 to i-1 do
                    str2:=str2+str[j];
                   mec[k].ec1:=str2;
                   str2:='';
                   for j:=i+1 to length(str) do
                    str2:=str2+str[j];
                   mec[k].ec2:=str2;
                   i:=length(str);
                  end;
 until eof(f);
 close(f);
 if corect(n,k)=false then
                       begin
                        writeln('Numarul meciurilor nu este corect.');
                        close(g);
                       end
 else
  begin
   j:=0;
   for i:=1 to k do
    begin
     if ok(mec[i].ec1,j) then
                          begin
                           j:=j+1;
                           s[j]:=mec[i].ec1;
                          end;
     if ok(mec[i].ec2,j) then
                          begin
                           j:=j+1;
                           s[j]:=mec[i].ec2;
                          end;
    end;
   for i:=1 to n do
    for j:=1 to n do
     mat[i,j]:=0;
   for i:=1 to k do
    begin
     for j:=1 to n do
      if s[j]=mec[i].ec1 then a:=j;
     for j:=1 to n do
      if s[j]=mec[i].ec2 then b:=j;
     mat[a,b]:=1;
    end;
   aranjare(1);
  end;
 readkey;
end.