--
-- Finally, a test to show that store based on precomputed static
-- links works as well. Final result: 321.
--

let a :: Int = 5
 ;  f :: Int -> Int =
	\x -> let b :: Int = 6
	       ;  g :: Int -> Int =
			\y -> let c :: Int = 100
				; h :: Int -> Int =
					\z -> let i :: Int -> Int = \t -> a
						in a := 1 ; b := 2 ; c := 3; 100*c + 10*b + 1*a
						ni
				in h 3
				ni
		in g 3
		ni
 in f 5
 ni
