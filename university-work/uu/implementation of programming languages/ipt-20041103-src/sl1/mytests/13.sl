--
-- This test proves that assignment to a tuple works as expected.
--

let a :: Int = 5 ;
    b :: Int = 5 ;
    c :: Int = 5
in (a, (b, c)) := (1, (2, 3)) ;
   a + b + c
ni
