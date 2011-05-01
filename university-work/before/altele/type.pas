Program pentru_invatat_tastatura;
uses crt, windos;
type fis=record
          g:string;
         end;
var i,n,j,k,l,y,gr,lit_s,cuv_m,min,sec,timp:integer;
    cuv_s:real;
    h1,m1,s1,h2,m2,s2,z:word;
    f:file of fis;
    c:char;
    s:fis;
    x:text;
    str:array[1..11] of string;

begin
 clrscr;
 writeln;
 textcolor(lightmagenta);
 writeln(' Acesta este un program cu ajutorul caruia veti invata mai usor sa');
 writeln('tastati ( fara sa va uitati la tastatura ). Programul contine 10 ');
 writeln('texte pe care dumneavoastra va trebui sa le tastati. Programul va');
 writeln('cronometra timpul in care veti tasta aceste texte si va va spune ');
 writeln('ulterior cate cuvinte puteti scrie pe minut.');
 writeln;
 textcolor(yellow);
 gotoxy(20,25);
 writeln('Apasati "ENTER" pentru continuare.');
 readln;
 clrscr;
 writeln;
 textcolor(red);
 assign(f,'type.dat');
 reset(f);
 write(' Ce text doriti sa tastati ? (1-10) ');readln(n);
 clrscr;
 seek(f,n-1);
 read(f,s);
 close(f);
 assign(x,s.g);
 reset(x);
 i:=1;
 while not eof(x) do
  begin
   readln(x,str[i]);
   writeln(str[i]);writeln;
   inc(i);
  end;
 l:=0;
 textcolor(yellow);
 for j:=1 to i do
  inc(l,length(str[j]));
 gettime(h1,m1,s1,z);
 gotoxy(1,2);
 k:=0;
 y:=0;
 i:=1;
 repeat
  c:=readkey;
  if c in [#32..#126] then
                       begin
                        inc(y);
                        if c=str[i][y] then write(c)
                                       else
                                        begin
                                         inc(gr);
                                         write(c);
                                         sound(250);
                                         delay(100);
                                         nosound;
                                        end;
                        if y=length(str[i]) then
                                             begin
                                              y:=0;
                                              inc(i);
                                              gotoxy(1,wherey+2);
                                             end;
                       end;

  inc(k);
 until k=l;
 clrscr;
 gettime(h2,m2,s2,z);
 if h2=h1 then
  if s2>s1 then
            begin
             min:=m2-m1;
             sec:=s2-s1;
            end
           else
            begin
             min:=m2-m1-1;
             sec:=60+s2-s1;
            end
 else
  if s2>s1 then
            begin
             min:=60+m2-m1;
             sec:=s2-s1;
            end
           else
            begin
             min:=60+m2-m1-1;
             sec:=60+s2-s1;
            end;
 timp:=min*60+sec;
 lit_s:=trunc(l/timp);
 cuv_s:=lit_s/5;
 cuv_m:=trunc(cuv_s*60);
 textcolor(blue);
 writeln(' Viteza dvs. de tiparire este de aproximativ ',cuv_m,' cuvinte/minut');
 timp:=trunc(100*gr/l);
 timp:=100-timp;
 writeln(' Acuratetea dvs. de tiparire este de aproximativ ',timp,'% .');
 readkey;
end.