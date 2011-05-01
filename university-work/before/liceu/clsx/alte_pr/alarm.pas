Program Help;
uses crt, dos;
label 1;
var hour, minute, sec, sec10 :word;
begin
 clrscr;
 writeln;
 randomize;
 repeat
 gettime(hour,minute,sec,sec10);
 if (hour=6) and (minute=0) then
                              begin
                               repeat
                                  sound(300);
                                  delay(1000);
                                  nosound;
                               until keypressed;
                              end;
 until keypressed;
1:
end.