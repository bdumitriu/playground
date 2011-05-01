--
-- This test proves the correct implementation of the realtional operators for
-- tuples of Int's.
--

let a :: (Int, Int, Int) = (4, 4, (5, 6)) ;
    b :: (Int, Int, Int) = (6, 7, (9, 9)) ;
    c :: (Int, Int, Int) = (4, 4, (5, 6))
in (a == b, a == c, a /= b, a /= c, a <= b, a <= c, a >= b, a >= c, a < b, a < c, a > b, a > c)
ni
