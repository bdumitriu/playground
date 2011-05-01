--
-- Test which shows (again) that sel works correctly and that an error message
-- is produced if trying to access too large an index usign sel.
--

let a :: (Int, Bool, Int) = (4, True || False, 4+5) ;
    b :: (Int, Bool, Int) = (6, True, 9) ;
    c :: Int = 5
in (sel 0 a, sel 1 b, sel 0 b, sel 2 a, sel 3 a)
ni
