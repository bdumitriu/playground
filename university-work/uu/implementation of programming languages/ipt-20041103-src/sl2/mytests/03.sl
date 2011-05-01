--
-- Now, I've added an extra variable, b, on level 0 and I'm accessing it
-- from level 3. Again, the static link is precomputed, since two accesses
-- to the same level are used. This test is provided to show that, even if
-- access to different variables in the same level is done, the code detects
-- that the static link needs to be precomputed.
--

let a :: Int = 5
 ;  b :: Int = 6
 ;  f :: Int -> Int =
	\x -> let g :: Int -> Int =
			\y -> let h :: Int -> Int = \z -> a + b
				in h 3
				ni
		in g 3
		ni
 in f 5
 ni
