Program editor_de_texte;
uses crt,mouse;
type xxx=array[1..10] of string;
var i,j:integer;
    c:char;
    test:boolean;
    cuv:xxx;
    str:array[1..240] of string;
    {f,g:text;}
    x:string;

Procedure meniu(cuv:xxx;x1,x2,y2:integer;var z:integer);
var i:integer;
begin
 window(x1,2,x2,y2);
 textbackground(lightgray);
 textcolor(black);
 clrscr;
end;

begin
 clrscr;
 {mon;}
 {assign(f,'prg.txt');
 assign(g,'help.txt');
 rewrite(f);}
 textbackground(lightgray);
 textcolor(red);
 write('  F');
 textcolor(black);
 write('isier');
 textcolor(red);
 write('  E');
 textcolor(black);
 write('ditare');
 textcolor(red);
 write('  C');
 textcolor(black);
 write('autare');
 textcolor(red);
 write('  A');
 textcolor(black);
 write('jutor                                              ');
 textbackground(blue);
 textcolor(white);
 write('É');
 for i:=2 to 79 do
  begin
   gotoxy(i,2);
   write('Í');
   gotoxy(i,24);
   write('Í');
  end;
 gotoxy(80,2);
 write('»');
 for i:=3 to 23 do
  begin
   gotoxy(1,i);
   write('º');
   gotoxy(80,i);
   write('º');
  end;
 gotoxy(1,24);
 write('È');
 gotoxy(80,24);
 write('¼');
 gotoxy(3,2);
 write('[');
 textcolor(lightgreen);
 write('ß');
 textcolor(white);
 write(']');
 textbackground(black);
 gotoxy(1,25);
 for i:=1 to 79 do
  write(' ');
 textbackground(blue);
 window(2,3,79,23);
 clrscr;
 window(1,1,80,25);
 gotoxy(1,25);
 textbackground(lightgray);
 textcolor(red);
 write(' F1 ');
 textcolor(black);
 write('Ajutor');
 textcolor(red);
 write('  F2 ');
 textcolor(black);
 write('Salvare');
 textcolor(red);
 write('  F3 ');
 textcolor(black);
 write('Deschidere de fisier                                ');
 window(2,3,79,23);
 gotoxy(1,1);
 textbackground(blue);
 textcolor(yellow);
 repeat
  c:=readkey;
  if c in [#32..#126] then
                       begin
                        write(c);
                        {write(f,c);}
                       end;
  if c=#13 then
            begin
             {gotoxy(1,wherey);
             readln(x);
             j:=0;
             for i:=1 to length(x) do
              if x[i]<>' ' then inc(j)
                           else i:=length(x);}
             gotoxy(1,wherey+1);
            { writeln(f);}
            end;
  if c=#8 then
           begin
            {close(f);
            reset(f);
            i:=0;
            repeat
             readln(f,str);
             inc(i);
            until i=wherey;
            test:=false;
            if length(str)=wherex-1 then
                                     begin
                                      str:=str+' ';
                                      test:=true;
                                     end;
            str[wherex-1]:=str[wherex];
            if test then str:=copy(str,1,length(str)-2)
                    else str:=copy(str,1,length(str)-1);
            close(f);
            reset(f);
            rewrite(g);
            j:=0;
            while j+1<i do
             begin
              readln(f,str2);
              writeln(g,str2);
             end;
            writeln(g,str);
            readln(f);
            while not eof(f) do
             begin
              readln(f,str);
              write(g,str);
             end;
            close(f);
            erase(f);
            close(g);
            assign(f,'help.txt');
            rename(f,'prg.txt');
            append(f);
            gotoxy(1,wherey);
            write(str,' ');
            gotoxy(wherex-1,wherey);     }
            gotoxy(wherex-1,wherey);
            write(' ');
            gotoxy(wherex-1,wherey);
           end;
  if c=#0 then
           begin
            c:=readkey;
            if c=#75 then gotoxy(wherex-1,wherey);
            if c=#77 then gotoxy(wherex+1,wherey);
            if c=#80 then gotoxy(wherex,wherey+1);
            if c=#72 then gotoxy(wherex,wherey-1);
            if c=#33 then
                      begin
                       meniu(cuv,1,30,19,i);
                      end;
           end;
 until c=#27;
 {close(f);
 erase(f);}
end.