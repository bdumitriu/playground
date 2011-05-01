Program de_test;
var a : array[1..500] of ^string;
    i : integer;

begin
   for i := 1 to 500 do
      new(a[i]);

end.