Program Subiectul_V;
uses crt;

type rep = ^el;
     el = record
             n: integer;
             next: rep;
          end;

var m, n, i: integer;
    f: text;
    a: array [1..100] of integer;
    cap, p, q, r: rep;

Procedure AdaugareElement(x, j: integer); {Procedura realizeaza plasarea}
begin                                     {elementului x in lista la locul}
   if j = 1 then                          {lui pt. a pastra lista ordonata}
   begin
      new(p);
      p^.next := nil;
      p^.n := x;
      cap^.next := p;
   end
   else
   begin
      q := cap^.next;
      r := cap;
      while x > q^.n do
      begin
         r := q;
         q := q^.next;
      end;
      new(p);
      p^.n := x;
      p^.next := q;
      r^.next := p;
   end;
end;

Procedure TiparesteLista;
begin
   p := cap^.next;
   if p <> nil then
   begin
      write(f, ' Lista ordonata este: ');
      while p <> nil do
      begin
         write(f, p^.n, ' ');
         p := p^.next;
      end;
   end
   else
      write(f, ' Lista este vida.');
end;

begin
   clrscr;
   writeln;

   assign(f, 'input.txt');
   reset(f);
   readln(f, n);
   close(f);
   assign(f, 'numere.txt');
   reset(f);

   i := 0;
   new(cap);
   cap^.next := nil;
   while i < n do       {se realizeaza citirea numerelor din fisier}
   begin
      inc(i);
      readln(f, m);
      AdaugareElement(m, i);
   end;

   close(f);
   assign(f, 'output.txt');
   rewrite(f);
   TiparesteLista;
   close(f);

   write(' Fisierul output.txt a fost scris.');
   readkey;
end.