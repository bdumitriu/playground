Program olimpiada_problema_2;
uses crt;
type sol=array[0..100] of record                {vi-vasul din care se toarna}
                        vi,vf,q1,q2,q3:integer; {vf-vasul in care se toarna}
                       end; {q1,q2,q3-cantitatea de apa ramasa in fiecare}
var f,g:text;               {din cele 3 vase dupa mutarea din vi in vf}
    c,p:array[1..3] of 0..1000;   {c-capacitatea fiecarui dintre cele 3 vase}
    cor:array[1..3] of char;      {p-cantitea de apa din fiecare vas}
    s:sol;                        {cor-corespondentul in litere a fiecarui}
    str:string;                   {din cele 3 vase}
    m,n,nr_mut:integer;  {nr_mut-numarul de mutari necesare}

Procedure scrie(pas:integer);
var x:integer;
begin
 writeln(g,nr_mut);
 for x:=1 to pas do
  writeln(g,cor[s[x].vi],' ',cor[s[x].vf]);
 close(g);
 halt;
end;

Function gata(pas:integer):boolean;
begin
 gata:=false;
 if (s[pas].q1=0) and (s[pas].q2=s[pas].q3) then gata:=true;
 if (s[pas].q2=0) and (s[pas].q1=s[pas].q3) then gata:=true;
 if (s[pas].q3=0) and (s[pas].q1=s[pas].q2) then gata:=true;
end;

Function ok(pas:integer):boolean;
var x,y,z:integer;
begin
 ok:=true;
 if s[pas].vi=s[pas].vf then ok:=false;
 if pas>1 then
  if (s[pas-1].vi=s[pas].vf) and (s[pas-1].vf=s[pas].vi) then ok:=false;
 if p[s[pas].vi]=0 then ok:=false;
 if p[s[pas].vf]=c[s[pas].vf] then ok:=false;
 for x:=0 to pas-1 do
  if (s[x].q1=s[pas].q1) and
     (s[x].q2=s[pas].q2) and
     (s[x].q3=s[pas].q3) then ok:=false;
end;

Procedure turnari(pas:integer);
var i,j:byte;
    ini,ini1,ini2,ini3:integer;
begin
 for i:=1 to 3 do
  for j:=1 to 3 do
   begin
    s[pas].vi:=i;
    s[pas].vf:=j;
    if ok(pas) then
     begin
      ini1:=p[1];
      ini2:=p[2];
      ini3:=p[3];
      nr_mut:=nr_mut+1;
      ini:=p[s[pas].vi];
      p[s[pas].vi]:=p[s[pas].vi]-(c[s[pas].vf]-p[s[pas].vf]);
      if p[s[pas].vi]<0 then p[s[pas].vi]:=0;
      p[s[pas].vf]:=p[s[pas].vf]+ini;
      if p[s[pas].vf]>c[s[pas].vf] then p[s[pas].vf]:=c[s[pas].vf];
      s[pas].q1:=p[1];
      s[pas].q2:=p[2];
      s[pas].q3:=p[3];
      if gata(pas) then scrie(pas)
                   else turnari(pas+1);
      p[1]:=ini1;
      p[2]:=ini2;
      p[3]:=ini3;
     end;
   end;
end;

begin
 clrscr;
 writeln;
 write('Introduceti numele fisierului de intrare : ');readln(str);
 assign(f,str);
 write('Introduceti numele fisierului de iesire : ');readln(str);
 assign(g,str);
 rewrite(g);
 reset(f);
 readln(f,m);
 readln(f,n);
 c[1]:=m;
 c[2]:=n;
 c[3]:=m+n;
 p[1]:=0;
 p[2]:=0;
 p[3]:=m+n;
 nr_mut:=0;
 cor[1]:='A';
 cor[2]:='B';
 cor[3]:='C';
 s[0].q1:=0;
 s[0].q2:=0;
 s[0].q3:=12;
 turnari(1);
 close(f);
 close(g);
end.