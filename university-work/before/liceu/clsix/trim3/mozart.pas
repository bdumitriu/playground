Program Mozart;
uses crt;
type sir=array[1..42] of word;
const frecv:sir=(392,294,392,294,392,294,392,494,588,524,440,
                 524,440,524,440,370,440,294,392,392,494,440,
                 392,392,370,370,440,524,370,440,392,392,494,
                 440,392,392,370,370,440,524,370,392);
      p:word=600;
      r:word=300;
      optime:word=450;
      patrime:word=900;
var i:byte;
begin
repeat
for i:=1 to 42 do
begin
sound(frecv[i]);
if i in [1,3,9,10,12,18,19,20,26,32,38,42]
then delay(patrime)
else delay(optime);
nosound;
if i in [1,3,10,12,20,26,32,38]
then delay(r);
if i in [9,18]
then delay(p)
end;
until keypressed
end.