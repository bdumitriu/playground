--
-- Now, b is moved on a different level than a, showing that static links
-- are now no longer precomputed, because we're only accessing one variable
-- in each of the 0th and 1st level.
--

let a :: Int = 5
 ;  f :: Int -> Int =
	\x -> let b :: Int = 6
		; g :: Int -> Int =
			\y -> let h :: Int -> Int = \z -> a + b
				in h 3
				ni
		in g 3
		ni
 in f 5
 ni
