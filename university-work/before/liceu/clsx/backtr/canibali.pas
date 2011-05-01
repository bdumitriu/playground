Program canibali_si_misionari;
uses crt;
var c,m,cs,ms,cd,md:integer;
    s:array[0..100,1..5] of integer;

Procedure scrie(i:integer);
begin
 for i:=0 to i do
  begin
   write(s[i,2]:2,s[i,3]:2,s[i,4]:5,s[i,5]:2);
   writeln;
  end;
 readln;
end;

Function ok(i:integer):boolean;
var k:byte;
    aux:boolean;
begin
 ok:=true;
 if i mod 2 = 1
  then
   case s[i,1] of
    1:begin
       s[i,2]:=s[i-1,2]-2;
       s[i,3]:=s[i-1,3]-0;
       s[i,4]:=s[i-1,4]+2;
       s[i,5]:=s[i-1,5]+0;
      end;
    2:begin
       s[i,2]:=s[i-1,2]-0;
       s[i,3]:=s[i-1,3]-2;
       s[i,4]:=s[i-1,4]+0;
       s[i,5]:=s[i-1,5]+2;
      end;
    3:begin
       s[i,2]:=s[i-1,2]-1;
       s[i,3]:=s[i-1,3]-1;
       s[i,4]:=s[i-1,4]+1;
       s[i,5]:=s[i-1,5]+1;
      end;
    4:begin
       s[i,2]:=s[i-1,2]-1;
       s[i,3]:=s[i-1,3]-0;
       s[i,4]:=s[i-1,4]+1;
       s[i,5]:=s[i-1,5]+0;
      end;
    5:begin
       s[i,2]:=s[i-1,2]-0;
       s[i,3]:=s[i-1,3]-1;
       s[i,4]:=s[i-1,4]+0;
       s[i,5]:=s[i-1,5]+1;
      end;
   end
  else
   case s[i,1] of
    1:begin
      s[i,2]:=s[i-1,2]+0;
      s[i,3]:=s[i-1,3]+2;
      s[i,4]:=s[i-1,4]-0;
      s[i,5]:=s[i-1,5]-2;
     end;
    2:begin
       s[i,2]:=s[i-1,2]+2;
       s[i,3]:=s[i-1,3]+0;
       s[i,4]:=s[i-1,4]-2;
       s[i,5]:=s[i-1,5]-0;
      end;
    3:begin
       s[i,2]:=s[i-1,2]+1;
       s[i,3]:=s[i-1,3]+1;
       s[i,4]:=s[i-1,4]-1;
       s[i,5]:=s[i-1,5]-1;
      end;
    4:begin
       s[i,2]:=s[i-1,2]+1;
       s[i,3]:=s[i-1,3]+0;
       s[i,4]:=s[i-1,4]-1;
       s[i,5]:=s[i-1,5]-0;
      end;
    5:begin
       s[i,2]:=s[i-1,2]+0;
       s[i,3]:=s[i-1,3]+1;
       s[i,4]:=s[i-1,4]-0;
       s[i,5]:=s[i-1,5]-1;
      end;
   end;
 if (s[i,2]<0) or (s[i,3]<0) or (s[i,4]<0) or (s[i,5]<0) then ok:=false;
 if (s[i,3]>0) and (s[i,2]>s[i,3]) then ok:=false;
 if (s[i,5]>0) and (s[i,4]>s[i,5]) then ok:=false;
 if (i>1) and (s[i,1]=s[i-1,1]) then ok:=false;
 for k:=0 to i-1 do
  if (i-k) mod 2 = 0 then
   if (s[k,2]=s[i,2]) and (s[k,3]=s[i,3]) and
      (s[k,4]=s[i,4]) and (s[k,5]=s[i,5]) then ok:=false;
end;

Procedure mutari(i:integer);
var j:byte;
begin
 for j:=1 to 5 do
  begin
   s[i,1]:=j;
   if ok(i) then
    if (s[i,2]=0) and (s[i,3]=0) then scrie(i)
                                 else mutari(i+1);
  end;
end;

begin
 clrscr;
 writeln;

 write(' Numarul de canibali : ');
 readln(c);
 write(' Numarul de misionari : ');
 readln(m);
 cs:= c;
 ms:= m;
 cd:= 0;
 md:= 0;
 s[0,2]:= cs;
 s[0,3]:= ms;
 s[0,4]:= cd;
 s[0,5]:= md;
 mutari(1);

 readkey;
end.