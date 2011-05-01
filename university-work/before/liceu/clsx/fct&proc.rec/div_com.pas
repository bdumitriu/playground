Program de_aflat_cel_mai_mare_divizor_comun_recursiv;
uses crt;
var a,b:integer;

function divizor(a,b:integer):integer;
begin
if a=b then divizor:=a
       else if a>b then divizor:=divizor(a-b,b)
                   else divizor:=divizor(a,b-a);
end;

begin
clrscr;
write('Introduceti primul numar : ');readln(a);
write('Introduceti al doilea numar : ');readln(b);
write('Cel mai mare divizor comun al numerelor ',a,' si ',b,' este ',divizor(a,b));
readkey;
end.