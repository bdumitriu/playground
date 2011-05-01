Program olimpiada_205P1;
uses crt;
var f:array[1..10] of text;
    g:text;
    pl,so:string[30];  {oras de plecare,oras de sosire}
    nr_fis:0..9;  {numarul de fisiere}
    i,j,k,l,m,n,code:integer;
    tren:array[1..10] of integer;  {numarul trenurilor}
    traseu:array[1..9,1..20] of record  {traseul cu orele pt. trenuri}
                                 oras:string[30];
                                 hh,mm:integer;
                                end;
    str,str2:string[40];
    test:boolean;

Function ok(h1,m1,h2,m2:integer):boolean;
begin
 ok:=true;
 if h2>h1 then
  begin
   if m2>=m1 then if h2-h1>3 then ok:=false;
   if m2<m1 then if h2-h1>4 then ok:=false;
  end;
 if h2<h1 then
  begin
   if m2>=m1 then if h2-h1>-21 then ok:=false;
   if m2<m1 then if h2-h1>-22 then ok:=false;
  end;
end;

begin
 clrscr;
 writeln;
 write('Introduceti numarul de fisiere : ');
 readln(nr_fis);
 write('Introduceti numele orasului de plecare : ');
 readln(pl);
 write('Introduceti numele orasului de sosire : ');
 readln(so);
 for i:=1 to nr_fis do
  begin
   case i of
   1:begin assign(f[i],'mers0.txt');reset(f[i]); end;
   2:begin assign(f[i],'mers1.txt');reset(f[i]); end;
   3:begin assign(f[i],'mers2.txt');reset(f[i]); end;
   4:begin assign(f[i],'mers3.txt');reset(f[i]); end;
   5:begin assign(f[i],'mers4.txt');reset(f[i]); end;
   6:begin assign(f[i],'mers5.txt');reset(f[i]); end;
   7:begin assign(f[i],'mers6.txt');reset(f[i]); end;
   8:begin assign(f[i],'mers7.txt');reset(f[i]); end;
   9:begin assign(f[i],'mers8.txt');reset(f[i]); end;
   10:begin assign(f[i],'mers9.txt');reset(f[i]); end;
   end;
   read(f[i],tren[i]);
   readln(f[i]);
   k:=1;
   while not eof(f[i]) do
    begin
     read(f[i],str);
     traseu[i,k].oras:='';
     for j:=1 to length(str) do
      begin
       if not ((str[j]=' ') and (str[j+1] in ['0'..'9'])) then
               traseu[i,k].oras:=traseu[i,k].oras+str[j];
       if (str[j]=' ') and (str[j+1] in ['0'..'9']) then
               begin
                if str[j+2]='.' then
                                 begin
                                  str2:=str[j+1];
                                  val(str2,traseu[i,k].hh,code);
                                  str2:=str[j+3];
                                  str2:=str2+str[j+4];
                                  val(str2,traseu[i,k].mm,code);
                                  j:=length(str);
                                 end;
                if str[j+3]='.' then
                                 begin
                                  str2:=str[j+1];
                                  str2:=str2+str[j+2];
                                  val(str2,traseu[i,k].hh,code);
                                  str2:=str[j+4];
                                  str2:=str2+str[j+5];
                                  val(str2,traseu[i,k].mm,code);
                                  j:=length(str);
                                 end;
               end;
      end;
     k:=k+1;
     readln(f[i]);
    end;
   close(f[i]);
  end;
  assign(g,'out.txt');
  rewrite(g);
  test:=true;
  for i:=1 to nr_fis do
   begin
    for j:=1 to 20 do
     if traseu[i,j].oras=so then
                             begin
                              for k:=1 to j do
                                if traseu[i,k].oras=pl then
                                 begin
                                  write(g,tren[i],' ',traseu[i,k].hh,'.',
                                  traseu[i,k].mm,' ',traseu[i,j].hh,'.',
                                  traseu[i,j].mm);
                                  test:=false;
                                  writeln(g);
                                 end;
                               j:=20;
                              end;
   end;
  if test then
    for i:=1 to nr_fis do
     begin
      for j:=1 to 20 do
       if traseu[i,j].oras=so then
                               begin
                                for k:=1 to j do
                                 for l:=1 to nr_fis do
                                  for m:=1 to 20 do
                                   {begin}
                                    if traseu[l,m].oras=pl then
                                     for n:=m to 20 do
                                      if (traseu[l,n].oras=traseu[i,k].oras)
                                      and (ok(traseu[l,n].hh,traseu[l,n].mm,
                                      traseu[i,k].hh,traseu[i,k].mm)) then
                                       begin
                              write(g,tren[l],' ',traseu[l,m].hh,
                              '.',traseu[l,m].mm,' ',traseu[l,n].oras,' ',
                              tren[i],' ',traseu[i,j].hh,'.',traseu[i,j].mm);
                              writeln(g);
                              n:=20;
                              m:=20;
                                       end;
                                j:=20;
                               end;
     end;
  close(g);
 readkey;
end.