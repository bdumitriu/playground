Program procente;
uses crt,utilg,graph;
label sf,d;
var n,i,s:integer;
    x:array[1..16] of integer;
    y:array[0..16] of integer;
begin
 clrscr;
 writeln;
 repeat
  begin
   write('Numarul de procente pe care doriti sa-l introduceti este (maxim 15) : ');
   readln(n);
  end;
 until (n<=15) and (n>0);
 writeln('Acum introduceti cele ',n,' procente (fara caracterul % dupa nr.):');
 writeln('ATENTIE, SUMA LOR TREBUIE SA FIE 100 SI NICIUNUL SA NU FIE <=0!!!');
 s:=0;
 for i:=1 to n do
  begin
   write('Procent[',i,'] = ');
   readln(x[i]);
   if x[i]<=0 then goto d;
   s:=s+x[i];
  end;
 d:
 if s<>100 then
  begin
   writeln('Executati programul inca o data, cititi instructiunile cu atentie ');
   writeln('si o sa va dati seama unde ati gresit !!!');
   readkey;
   goto sf;
  end;
 y[0]:=0;
 for i:=1 to n do
  y[i]:=((x[i]*360) div 100)+y[i-1];
 ini;
 setfillstyle(solidfill,white);
 pieslice(320,240,0,y[1],200);
 for i:=2 to n-1 do
  begin
   setfillstyle(i,i-1);
   pieslice(320,240,y[i-1],y[i],200);
  end;
 setfillstyle(n-1,n);
 pieslice(320,240,y[n-1],360,200);
 readkey;
 closegraph;
 sf:
end.