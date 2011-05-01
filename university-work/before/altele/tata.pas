{$M 8192,0,0}
Program pentru_tata_LOTO_PRONO;
uses crt, dos;
label 1;
var n:6..49;
    f:text;
    c:char;
    m,nr:integer;
    num:array[1..49] of integer;
    s:array[1..6] of integer;
    mat:array[1..1000,1..6] of integer;

Function nt(n:integer):longint;
var x:longint;
begin
 x:=n;
 x:=x*(n-1);
 x:=x div 2;
 x:=x*(n-2);
 x:=x div 3;
 x:=x*(n-3);
 x:=x div 4;
 x:=x*(n-4);
 x:=x div 5;
 x:=x*(n-5);
 x:=x div 6;
 nt:=x;
end;

Procedure scrie(c:char);
var x:integer;
begin
 m:=m+1;
 write('*');
 for x:=1 to 6 do
  mat[m,x]:=s[x];
 nr:=nr+1;
 writeln(f,'Varianta ',nr,' : ');
 for x:=1 to 6 do
  write(f,num[s[x]],' ');
 writeln(f);
end;

Function ok(i:integer):boolean;
var x,y,z,a:integer;
    aux:boolean;
begin
 ok:=true;
 aux:=true;
 for x:=1 to i-1 do
  if s[i]<=s[x] then
                 begin
                  ok:=false;
                  aux:=false;
                 end;
 if i=6 then
  begin
   a:=0;
   for x:=1 to m do
    begin
     a:=0;
     for y:=1 to 6 do
      for z:=1 to 6 do
       if mat[x,y]=s[z] then a:=a+1;
     if a>=5 then
            begin
             ok:=false;
             aux:=false;
            end;
    end;
  end;
end;

Procedure combinari(i:integer;c:char);
var j:integer;
begin
 for j:=1 to n do
  begin
   s[i]:=j;
   if ok(i) then
    if i=6 then scrie(c)
           else combinari(i+1,c);
  end;
end;

begin
 repeat
  clrscr;
  textcolor(lightblue);
  gotoxy(20,12);
  write('Numarul de numere cu care se va opera : ');
  gotoxy(20,13);
  textcolor(red);
  write('           n intre 6 si 49');
  gotoxy(60,12);
  textcolor(lightgreen);
  read(n);
  window(1,1,80,25);
 until (n>5) and (n<50);
 clrscr;
 textcolor(brown);
 writeln;
 writeln('Introduceti cele ',n,' numere : ');
 for m:=1 to n do
  begin
   1:textcolor(brown);
   write('Numarul ',m,' : ');
   textcolor(red);
   readln(num[m]);
   for nr:=1 to m-1 do
    if num[m]=num[nr] then
                       begin
                        write('Acest numar a mai fost introdus o data.');
                        writeln('Reintroduceti-l.');
                        goto 1;
                       end;
   if (num[m]>49) or (num[m]<1) then
                                 begin
                                  write('Numarul nu se afla in intervalul');
                                  writeln(' [1,49].Reintroduceti-l.');
                                  goto 1;
                                 end;
  end;
 clrscr;
 gotoxy(20,12);
 textcolor(yellow);
 write('Numarul total de variante este ',nt(n),'.');
 textcolor(lightgray);
 gotoxy(20,25);
 write('Apasati "ENTER" pentru continuare.');
 readln;
 clrscr;
 gotoxy(10,12);
 textcolor(yellow);
 write('Doriti rezultatele afisate si la imprimanta ? (d/n) : ');
 readln(c);
 textcolor(lightgray);
 assign(f,'loto.dat');
 rewrite(f);
 clrscr;
 m:=0;
 nr:=0;
 combinari(1,c);
 clrscr;
 writeln('Numarul redus de variante este ',nr,'.');
 writeln('Apasati "ENTER" pt. continuare.');
 readln;
 if c='d' then
           begin
            clrscr;
            swapvectors;
            exec('C:\COMMAND.COM','/c copy loto.dat >prn');
            swapvectors;
           end;
 close(f);
 readkey;
end.