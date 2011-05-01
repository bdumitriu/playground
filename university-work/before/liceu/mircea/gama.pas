uses crt;
procedure si_(i:longint);
begin
sound(247);
delay(i*100);
nosound;
end;
procedure doo(i:longint);
begin
sound(262);
delay(i*100);
nosound;
end;
procedure re(i:longint);
begin
sound(294);
delay(i*100);
nosound;
end;
procedure mi(i:longint);
begin
sound(330);
delay(i*100);
nosound;
end;
procedure fa(i:longint);
begin
sound(350);
delay(i*100);
nosound;
end;
procedure sol(i:longint);
begin
sound(392);
delay(i*100);
nosound;
end;
procedure la(i:longint);
begin
sound(428);
delay(i*100);
nosound;
end;

procedure doi;
begin
sol(3);sol(3);sol(2);fa(2);mi(2);doo(6);sol(3);sol(3);sol(2);mi(2);sol(2);la(2);
sol(3);fa(3);mi(3);re(3);doo(3);re(6);
sol(3);sol(3);sol(3);fa(3);mi(3);doo(3);
end;

procedure trei;
begin
fa(3);fa(3);fa(3);fa(4);mi(6);doo(4);fa(6);mi(4);mi(4);re(6);
fa(3);fa(3);fa(3);fa(4);mi(6);doo(3);doo(3);fa(4);mi(4);re(6);
fa(3);fa(3);fa(3);fa(4);mi(6);doo(3);fa(3);fa(3);mi(3);re(6);re(3);doo(3);mi(6);
fa(6);mi(4);fa(6);sol(4);sol(6);fa(6);mi(6);fa(3);mi(3);re(4);doo(4);doo(6);
end;

begin
 doi;
end.