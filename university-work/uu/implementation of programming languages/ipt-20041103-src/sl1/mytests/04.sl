--
-- This test illustrates the availability and correct functioning of fst, snd
-- and sel.
--
-- Note that the first 6 values of the resulting tuple are displayed correctly,
-- while the 7th generates an error since we are trying to apply snd to a
-- non-tuple value. It should be clear that the message would've been the same
-- if fst had been used instead of snd (since their implementation is similar).
--

let a :: (Int, Bool, Int) = (4, True || False, 4+5) ;
    b :: (Int, Bool, Int) = (6, True, 9) ;
    c :: (Int, Bool, Int, Int) = (3+4, True || True, 10, 11) ;
    d :: Int = 5
in (snd a, fst b, sel 0 c, sel 1 c, sel 2 c, sel 3 c, snd d)
ni
