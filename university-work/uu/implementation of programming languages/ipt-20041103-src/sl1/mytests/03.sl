--
-- A basic test which proves that tuples work. Note the imbricated tuple types
-- and tuple values as well as the fact that the expressions in the tuples are
-- correctly evaluated.
--
-- Also shows that the pretty printing of tuples works correctly.
--

let a :: (Int, Bool, (Int, Int)) = (4, True || False, (4+5, 7)) ;
    b :: (Int, Bool, Int) = (6, (True && False) || True, 9)
in (a, (a, b))
ni
