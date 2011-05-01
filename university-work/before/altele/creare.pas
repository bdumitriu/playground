uses crt;
type fis=record
          g:string;
         end;
var f:file of fis;
    x:fis;

begin
 clrscr;
 writeln;
 assign(f,'type.dat');
 reset(f);
 seek(f,filesize(f));
 write(' Numele fisierului : ');
 readln(x.g);
 write(f,x);
 close(f);
 clrscr;
end.