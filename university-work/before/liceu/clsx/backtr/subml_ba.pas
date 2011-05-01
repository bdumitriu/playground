Program submultimi_cu_k_elemente_ale_multimii_M_cu_n_elemente;
uses crt;
var i,k,n,j:integer;
    M:array[1..30] of char;
    s:array[1..100] of integer;
    cont:boolean;

Procedure scrie;
begin
 for j:=1 to k do
  write(M[s[j]],' ');
 readln;
end;

begin
 clrscr;
 writeln;
 write('Numarul de elemente a multimii : ');readln(n);
 for i:=1 to n do
  begin
   write('Elementul ',i,' al multimii M : ');readln(M[i]);
  end;
 write('Numarul de elemente ale submultimilor : ');readln(k);
 i:=1;
 s[i]:=0;
 repeat
  while s[i]<n do
   begin
    s[i]:=s[i]+1;
    cont:=true;
    for j:=1 to i-1 do if s[i]<=s[j] then cont:=false;
    if cont then
     if i=k then scrie
            else
             begin
              i:=i+1;
              s[i]:=0;
             end;
   end;
  i:=i-1;
 until i=0;
 readkey;
end.