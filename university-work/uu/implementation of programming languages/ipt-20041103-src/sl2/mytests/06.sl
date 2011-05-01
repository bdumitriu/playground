--
-- And a test with 5 levels, with access to 3 different levels. The (correctly)
-- computed result is 228.
--

let a :: Int = 5
 ;  f :: Int -> Int =
	\x -> let b :: Int = 6
	       ;  g :: Int -> Int =
			\y -> let c :: Int = 100
				; h :: Int -> Int =
					\z -> let i :: Int -> Int = \t -> t + a + b + a + b + b + c + c
						in i 0
						ni
				in h 3
				ni
		in g 3
		ni
 in f 5
 ni
