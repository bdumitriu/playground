--
-- This test is to show that 2 levels are accessed correctly as well. If there
-- would be a mixup of levels the final result would be incorrect (since b is
-- added 3 times and a only 2 times). But, as you can see, the result is correct
-- (i.e. 28). Again, static links for the 0th and 1st level are precomputed.
--

let a :: Int = 5
 ;  f :: Int -> Int =
	\x -> let b :: Int = 6
		; g :: Int -> Int =
			\y -> let h :: Int -> Int = \z -> a + a + b + b + b
				in h 3
				ni
		in g 3
		ni
 in f 5
 ni
