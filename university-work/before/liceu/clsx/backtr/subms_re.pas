Program submultimi_ale_caror_elemente_insumate_dau_un_numar_n;
uses crt;
var n,k,i,l:integer;
    M,s:array[1..100] of integer;

Procedure scrie(i:byte);
var x:integer;
begin
 for x:=1 to i do
  write(M[s[x]],' ');
 readln;
end;

Function suma(i:byte):integer;
var x:byte;
    p:integer;
begin
 p:=0;
 for x:=1 to i do
   p:=p+M[s[x]];
 suma:=p;
end;

Procedure subm(i:byte);
var j:byte;
    cont:boolean;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   cont:=true;
   for l:=1 to i-1 do if s[i]<=s[l] then cont:=false;
   if cont then
    if suma(i)=k then scrie(i)
                 else if suma(i)<k then subm(i+1);
 end;
end;

begin
 clrscr;
 writeln;
 write('Numarul de elemente al multimii este : ');readln(n);
 for i:=1 to n do
  begin
   write('Elementul ',i,' al multimii M : ');readln(M[i]);
  end;
 write('Suma ce trebuie obtinuta este : ');readln(k);
 subm(1);
 readkey;
end.