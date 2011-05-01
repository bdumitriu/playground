{$M 8192,0,0}
Program ddd;
uses crt,dos,windos;
begin
 clrscr;
 swapvectors;
 exec('c:\command.com','/c copy c:\tp\bin\temp\loto.dat >prn');
 swapvectors;
end.