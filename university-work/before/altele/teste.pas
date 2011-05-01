Program facut_luni_noaptea;
uses crt;
var a:string;
    c:char;
begin
 textbackground(cyan);
 clrscr;
 textcolor(black);
 gotoxy(15,13);
 write('ENTER PASSWORD PLEASE : ');
 window(39,13,70,13);
 textbackground(yellow);
 clrscr;
 c:=readkey;
 while c<>#13 do
  begin
   if c in [#32..#126] then
                        begin
                         write('*');
                         a:=a+c;
                        end;
   if c=#8 then
            begin
             gotoxy(wherex-1,wherey);
             write(' ');
             gotoxy(wherex-1,wherey);
             a:=copy(a,1,length(a)-1);
            end;
   c:=readkey;
  end;
 if a='THERE CAN BE ONLY ONE!' then
  begin
   window(1,1,80,25);
   textbackground(black);
   textcolor(white);
   clrscr;
   write('You knew the password, so what?  NOTHING HAPPENS!');
  end
 else
  begin
   window(1,1,80,25);
   textcolor(128);
   gotoxy(33,15);
   write(' WRONG PASSWORD ! ');
   textcolor(yellow);
   gotoxy(20,16);
   write(' PLEASE CHECK YOUR PASSWORD AND TRY AGAIN !!!');
  end;
 readkey;
end.