--
-- Test which shows (again) that the !! operator works correctly.
--

let a :: (Int, Bool, Int) = (4, True || False, 4+5) ;
    b :: (Int, Bool, Int) = (6, True, 9) ;
    c :: Int = 5
in (a !! 0, b !! 1, b !! 0, a !! 2)
ni
