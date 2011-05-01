--
-- This test shows that nesting of fst, snd and sel works as well.
--

fst (snd (sel 0 ((1,(3,2)),4)))
