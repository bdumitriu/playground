Program Arbori;

{In cadrul procedurii ArboreBinar se gasesc procedurile de creare a arbori-
lor binari si de parcurgere a lor in in-, pre- si postordine. In cadrul pro-
cedurii ArboreBinarDeCautare se gasesc procedurile de creare a arborilor
binari de cautare, de cautare a unui nod ce contine o anumita informatie, de
listare a arborelui in inordine (=> listare in ordine crescatoare) si de
stergere a unui nod al sau ce contine o anumita informatie.}

uses crt;

type reper = ^nod;
     nod = record
              s, d: reper;
              n: integer;
           end;

Procedure ArboreBinar;

var p, rad: reper;

Procedure creare(var p: reper);
var n: integer;
begin
   write('n = ');
   readln(n);
   if n <> 0 then
   begin
      new(p);
      p^.n := n;
      creare(p^.s);
      creare(p^.d);
   end
   else
      p := nil;
end;

Procedure inordine(p: reper);
begin
   if p <> nil then
   begin
      inordine(p^.s);
      write(p^.n, ' ');
      inordine(p^.d);
   end;
end;

Procedure preordine(p: reper);
begin
   if p <> nil then
   begin
      write(p^.n, ' ');
      preordine(p^.s);
      preordine(p^.d);
   end;
end;

Procedure postordine(p: reper);
begin
   if p <> nil then
   begin
      postordine(p^.s);
      postordine(p^.d);
      write(p^.n, ' ');
   end;
end;

begin
   rad := nil;
   creare(rad);
   write(' Arborele in inordine: ');
   inordine(rad);
   writeln;
   write(' Arborele in preorine: ');
   preordine(rad);
   writeln;
   write(' Arborele in postordine: ');
   postordine(rad);
   writeln;
end;

Procedure ArboreBinarDeCautare;

var p, q, rad: reper;
    n: integer;
    c: char;

Procedure cr(n: integer);
var test: boolean;
begin
   if rad = nil then
   begin
      new(rad);
      rad^.s := nil;
      rad^.d := nil;
      rad^.n := n;
   end
   else
   begin
      q := rad;
      test := true;
      repeat
         if n = q^.n then
            test := false;
         if n < q^.n then
            if q^.s = nil then
            begin
               new(p);
               p^.n := n;
               p^.s := nil;
               p^.d := nil;
               q^.s := p;
               test := false;
            end
            else
               q := q^.s
         else
            if n > q^.n then
               if q^.d = nil then
               begin
                  new(p);
                  p^.n := n;
                  p^.s := nil;
                  p^.d := nil;
                  q^.d := p;
                  test := false;
               end
               else
                  q := q^.d;
      until test = false;
   end;
end;

Procedure creare;
begin
   rad := nil;
   write('n (0 pt. terminare) = ');
   readln(n);
   while n <> 0 do
   begin
      cr(n);
      write('n (0 pt. terminare) = ');
      readln(n);
   end;
end;

Procedure listare(p: reper);
begin
   if p <> nil then
   begin
      listare(p^.s);
      write(p^.n, ' ');
      listare(p^.d);
   end;
end;

Function cautare(n: integer; var r: reper; var x: char): reper;

{Functia returneaza adresa nodului ce contine informatia n sau nil in cazul
in care nu o gaseste. In r se va depune adresa parintelui lui p sau nil in
cazul in care p e radacina sau nu se gaseste nici un nod care sa contina
informatia n. In x se va depune 's' daca r^.s = p respectiv 'd' daca r^.d = p}

var test: boolean;
begin
   if rad = nil then
   begin
      cautare := rad;
      r := nil;
      x := ' ';
   end
   else
   begin
      test := true;
      p := rad;
      r := nil;
      x := ' ';
      repeat
         if n = p^.n then
         begin
            cautare := p;
            test := false;
         end;
         if n < p^.n then
            if p^.s = nil then
            begin
               r := nil;
               x := ' ';
               cautare := nil;
               test := false;
            end
            else
            begin
               r := p;
               x := 's';
               p := p^.s;
            end
         else
            if n > p^.n then
               if p^.d = nil then
               begin
                  r := nil;
                  x := ' ';
                  cautare := nil;
                  test := false;
               end
               else
               begin
                  r := p;
                  x := 'd';
                  p := p^.d;
               end;
      until test = false;
   end;
end;

Procedure Stergere(n: integer);
var x: char;

Procedure FaraFii(x: char);
begin
   if p <> rad then
   begin
      if x = 's' then
         q^.s := nil;
      if x = 'd' then
         q^.d := nil;
      dispose(p);
   end
   else
      rad := nil;
end;

Procedure FiuDrept(x: char);
begin
   if p <> rad then
   begin
      if x = 's' then
         q^.s := p^.d;
      if x = 'd' then
         q^.d := p^.d;
      dispose(p);
   end
   else
   begin
      rad := rad^.d;
      dispose(p);
   end;
end;

Procedure FiuStang(x: char);
begin
   if p <> rad then
   begin
      if x = 's' then
         q^.s := p^.s;
      if x = 'd' then
         q^.d := p^.s;
      dispose(p);
   end
   else
   begin
      rad := rad^.s;
      dispose(p);
   end;
end;

Procedure DoiFii(x: char);
var tq: reper;
begin
   if p^.d^.s = nil then
   begin
      if p <> rad then
      begin
         if x = 's' then
            q^.s := p^.d;
         if x = 'd' then
            q^.d := p^.d;
         p^.d^.s := p^.s;
         dispose(p);
      end
      else
      begin
         q := p^.d;
         q^.s := p^.s;
         rad := q;
         dispose(p);
      end;
   end
   else
   begin
      q := p^.d;
      tq := p;
      while q^.s <> nil do
      begin
         tq := q;
         q := q^.s;
      end;
      p^.n := q^.n;
      tq^.s := q^.d;
      dispose(q);
   end;
end;

begin
   p := cautare(n, q, x);
   if p = nil then
   begin
      write(' Nodul nu exista in arbore.');
      writeln;
   end
   else
      if p^.s = nil then
         if p^.d = nil then
            FaraFii(x)
         else
            FiuDrept(x)
      else
         if p^.d = nil then
            FiuStang(x)
         else
            DoiFii(x);
end;

begin
   creare;
   write('Arborele in inordine: ');
   listare(rad);
   writeln;
   {write('Nodul de cautat (0 pt. terminare) = ');
   readln(n);
   while n <> 0 do
   begin
      if cautare(n, q, c) <> nil then
         writeln(' Nodul exista in arbore.')
      else
         writeln(' Nodul nu exista in arbore.');
      write('Urmatorul nod de cautat (0 pt. terminare) = ');
      readln(n);
   end;}
   write('Nodul de sters (0 pt. terminare) = ');
   readln(n);
   while n <> 0 do
   begin
      Stergere(n);
      repeat
         write(' Doriti afisarea arborelui in noua configuratie? (d/n) ');
         readln(c);
      until (c = 'd') or (c = 'n');
      if c = 'd' then
      begin
         write('  Noul arbore: ');
         listare(rad);
         writeln;
      end;
      write('Urmatorul nod de sters (0 pt. terminare) = ');
      readln(n);
   end;
end;

begin
   clrscr;
   ArboreBinarDeCautare;
end.