Program problem_E_de_pe_fise(Trains);
uses crt;
var f: text;
    j, k, d, v1, v2, t1, t2, tf: integer;
    a, b: array[1..1000] of integer;

begin
   clrscr;
   writeln;
   assign(f, 'e.dat');
   reset(f);
   readln(f, d, v1, v2, t1, t2, tf);
   k:= 0;
   for i:= 0 to tf do
      if i mod t1 = 0 then
       begin
        inc(k);
        a[k]:= v1*tf;
        if a[k] >= d then a[k]:= d;
       end;
   j:= 0;
   for i:= 0 to tf do
      if i mod t2 = 0 then
       begin
        inc(j);
        a[j]:= v2*tf;
        if a[j] >= d then a[j]:= d;
       end;
   r:= 0;
   for i:= 1 to k do
    begin
       r:=
    end;
   readkey;
end.
