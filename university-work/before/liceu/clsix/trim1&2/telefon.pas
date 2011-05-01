Program telefon;
uses crt;
var nume:string;
begin
clrscr;
Writeln('Acesta este un program care va va spune numerele de telefon a unor');
Writeln('persoane daca veti introduce numele acestora.Persoanele ale caror');
Writeln('numere de telefon sunt in memorie sunt:Fobis-Body Building,Neamtu');
Writeln('Victor,Dumitriu Ana,Nicoara Titus senior,Neamtu Sandu,Nicoara Marius,');
Writeln('Nicoara Sanda-servici,Radiotel,Avram Mihai,Avram Sandu,Dumitriu');
Writeln('Sorina-servici,Chiorean Horia,Iancu Tudor,Szekely Flaviu,Secara');
Writeln('Adrian,Lupas Gelu,Furnea Raluca,Ciupa Ilinca,Mircea Dana,Pop An-');
Writeln('dreea,d-na Onojescu,d-na Iepure,Memo-service,fam. Teius-Floti,Dumitriu');
Writeln('Radu.');
Writeln('');
Writeln('Pentru a obtine numarul dorit va rog sa introduceti numele exact ');
Writeln('cum este scris mai sus.In caz contrar computer-ul va afisa mesajul');
Writeln('"Acest numar nu este in memorie".Acelasi mesaj va fi afisat si in');
Writeln('cazul in care numele dat de d-voastra nu face parte din cele amin-');
Writeln('tite mai sus');
Writeln('');
Writeln('Introduceti va rog numele');
Write('nume=');read(nume);
if nume='Dumitriu Ana' then writeln('numarul de telefon este 188216')
else if nume='Neamtu Victor' then writeln('numarul de telefon este 186110')
else if nume='Nicoara Titus senior' then writeln('numarul de telefon este 199711')
else if nume='Neamtu Sandu'then writeln('numarul de telefon este 185429')
else if nume='Nicoara Marius'then writeln('numarul de telefon este 191887')
else if nume='Nicoara Sanda-servici'then writeln('numarul de telefon este 414062')
else if nume='Radiotel' then writeln('numarul de telefon este 414175')
else if nume='Avram Mihai' then writeln('numarul de telefon este 414181')
else if nume='Avram Sandu' then writeln('numarul de telefon este 110584')
else if nume='Dumitriu Sorina-servici' then writeln('numarul de telefon este 196890')
else if nume='Chiorean Horia' then writeln('numarul de telefon este 180628')
else if nume='Iancu Tudor' then writeln('numarul de telefon este 180834')
else if nume='Szekely Flaviu' then writeln('numarul de telefon este 187617')
else if nume='Secara Adrian' then begin
                             writeln('numarul de telefon este 197455 sau');
                             writeln('192321 192322 192323 192324-int.2007');
                             end
else if nume='d-na Onojescu' then writeln('numarul de telefon este 155256')
else if nume='d-na Iepure' then writeln('numarul de telefon este 157523')
else if nume='Lupas Gelu' then writeln('numarul de telefon este 414265')
else if nume='Furnea Raluca' then writeln('numarul de telefon este 159274')
else if nume='Mircea Dana' then writeln('numarul de telefon este 186224')
else if nume='Ciupa Ilinca' then writeln('numarul de telefon este 184291')
else if nume='Pop Andreea' then writeln('numarul de telefon este 167226')
else if nume='Fobis-Body Building' then writeln('numarul de telefon este 190799')
else if nume='fam. Teius-Floti' then writeln('numarul de telefon este 183083')
else if nume='Memo-service' then writeln('numarul de telefon este 156815')
else if nume='Dumitriu Radu' then writeln('numarul de telefon este 195656')
else writeln('Acest numar nu este in memorie');
writeln('press any key to continue');
while not keypressed do;
end.