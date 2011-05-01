Program de_scriere_a_unui_cuvant_invers;
uses crt;

Procedure cuv_inv;
var a:char;
begin
 read(a);
 if a<>' ' then cuv_inv;
 write(a);
end;

begin
 clrscr;
 writeln;
 write('Cuvantul terminat cu spatiu este ');
 cuv_inv;
 readkey;
end.