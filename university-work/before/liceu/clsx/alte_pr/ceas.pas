Program get;
uses crt,windos;
var a,b,c,d:word;
begin
 clrscr;
 repeat
  gettime(a,b,c,d);
  writeln('Ora este ',a,',',b,' minute ',c,' secunde');
  gotoxy(1,wherey-1);
 until keypressed;
 readkey;
end.