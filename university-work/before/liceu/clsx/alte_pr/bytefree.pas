Program biti_liberi;
uses crt, dos;
var dis:longint;

begin
 clrscr;
 writeln;
 dis:=diskfree(3);
 textcolor(magenta);
 writeln(' Spatiul liber pe disc este de ',dis,' biti.');
 write('  For Christ''s sake, free some space !!!');
 readkey;
end.