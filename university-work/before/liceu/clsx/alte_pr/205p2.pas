Program olimpiada_205P2;
uses crt;
var f,g:text;
    str,str2:string;  {variabile auxiliare pt. a citi din fisiere}
    n:0..50;  {numarul de orase}
    loc_ini:string[30];  {localitatea initiala}
    test:boolean;
    i,j,k,l,aux,code:integer;
    min:longint;
    comun:array[1..30] of record
                           oras1,oras2:string;
                           cost:0..30000;
                          end;
    s,fin:array[1..30] of integer;

Function gata:boolean;
var x:integer;
begin
 gata:=true;
 for x:=1 to n-2 do
  begin
   if comun[fin[i]].oras1=comun[fin[i+1]].oras2 then
    if comun[fin[i]].oras2>comun[fin[i+1]].oras2 then gata:=false;
   if comun[fin[i]].oras1>comun[fin[i+1]].oras2 then gata:=false;
  end;
end;

Function suma:longint;
var x:integer;
    aux:longint;
begin
 aux:=0;
 for x:=1 to n-1 do
  aux:=aux+comun[s[x]].cost;
 suma:=aux;
end;

Function ok(i:byte;loc:string):boolean;
var x:integer;
begin
 ok:=true;
 if not ((comun[s[i]].oras1=loc) or (comun[s[i]].oras2=loc)) then ok:=false;
 for x:=1 to i-1 do
  if s[i]=s[x] then ok:=false;
 for x:=1 to i-2 do
 if (comun[s[x]].oras1=comun[s[i]].oras1) or
    (comun[s[x]].oras1=comun[s[i]].oras2) or
    (comun[s[x]].oras2=comun[s[i]].oras1) or
    (comun[s[x]].oras2=comun[s[i]].oras2) then ok:=false;
end;

Procedure transmisie(i:byte;loc:string);
var j:integer;
    loc2:string;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if comun[j].oras1=loc then loc2:=comun[j].oras2
                         else loc2:=comun[j].oras1;
   if ok(i,loc) then
    if i<n-1 then transmisie(i+1,loc2)
             else if min>suma then
                               begin
                                fin:=s;
                                min:=suma;
                               end;
  end;
end;

begin
 clrscr;
 writeln;
 write('Introduceti numele fisierului de intrare : ');
 readln(str);
 assign(f,str);
 reset(f);
 write('Introduceti numele fisierului de iesire : ');
 readln(str);
 assign(g,str);
 read(f,str);
 str2:='';
 for i:=1 to length(str) do
  if not (str[i]=' ') then str2:=str2+str[i]
                    else
                     begin
                      val(str2,n,code);
                      str2:='';
                      for j:=i+1 to length(str) do
                       str2:=str2+str[j];
                      i:=length(str);
                     end;
 loc_ini:=str2;
 readln(f);
 k:=1;
 while not eof(f) do
  begin
   read(f,str);
   comun[k].oras1:='';
   comun[k].oras2:='';
   test:=true;
   for i:=1 to length(str) do
    begin
     if test then
      begin
       if not (str[i]=' ') then comun[k].oras1:=comun[k].oras1+str[i];
       if str[i]=' ' then
                      begin
                       test:=false;
                       i:=i+1;
                      end;
      end;
     if not test then
      begin
       if not (str[i]=' ') then comun[k].oras2:=comun[k].oras2+str[i];
       if str[i]=' ' then
                      begin
                       str2:='';
                       for j:=i+1 to length(str) do
                        str2:=str2+str[j];
                       val(str2,comun[k].cost,code);
                       i:=length(str);
                      end;
      end;
    end;
   readln(f);
   k:=k+1;
  end;
 close(f);
 for i:=1 to k-1 do
  min:=min+comun[i].cost;
 transmisie(1,loc_ini);
 rewrite(g);
 repeat
  for i:=1 to n-2 do
   begin
   if comun[fin[i]].oras1=comun[fin[i+1]].oras2 then
    if comun[fin[i]].oras2>comun[fin[i+1]].oras2 then
     begin
      aux:=fin[i];
      fin[i]:=fin[i+1];
      fin[i+1]:=aux;
     end;
    if comun[fin[i]].oras1>comun[fin[i+1]].oras2 then
     begin
      aux:=fin[i];
      fin[i]:=fin[i+1];
      fin[i+1]:=aux;
     end;
    end;
  until gata;
  write(g,min);
  writeln(g);
  for i:=1 to n-1 do
   begin
    write(g,comun[fin[i]].oras1,' ',comun[fin[i]].oras2);
    writeln(g);
   end;
  close(g);
 readkey;
end.