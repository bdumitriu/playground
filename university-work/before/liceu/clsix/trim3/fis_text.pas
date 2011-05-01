Program fisier_text;
var f:text;
    c:char;
begin
assign(f,'fis.txt');
rewrite(f);
write('Introduceti textul : ');read(c);
while c<>'.' do begin
write(f,c);
read(c);
end;
close(f);
end.
