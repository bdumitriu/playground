Program testare_daca_o_expresie_e_corecta_sau_nu;
uses crt;
const op_ad=['+','-'];
      op_mul=['*','/'];

var e:string;
    i,l:byte;

Procedure s; forward;

Procedure f;
begin
 if not (e[i] in ['a'..'z']) then
                              if e[i]='(' then
                                           begin
                                            i:=i+1;
                                            s;
                                            i:=i+1;
                                            if e[i]<>')' then i:=i-1;
                                           end
                                          else i:=i-1;
 end;

Procedure t;
begin
 f;
 if e[i+1] in op_mul then
                      begin
                       i:=i+2;
                       t;
                      end;
end;

Procedure s;
begin
 t;
 if e[i+1] in op_ad then
                     begin
                      i:=i+2;
                      s;
                     end;
end;

begin
 clrscr;
 writeln;
 write('Expresia : ');readln(e);
 i:=1;
 l:=length(e);
 s;
 if i=l then writeln('Expresia e corecta.')
        else writeln('Expresia nu e corecta.');
 readkey;
end.