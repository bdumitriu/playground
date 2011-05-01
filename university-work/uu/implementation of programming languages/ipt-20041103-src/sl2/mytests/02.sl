--
-- Almost same test as the previous one, with the exception that now,
-- in the body of h, we access variable a twice. Therefore, this time
-- the necessary static link is precomputed and then used twice.
--

let a :: Int = 5
 ;  f :: Int -> Int =
	\x -> let g :: Int -> Int =
			\y -> let h :: Int -> Int = \z -> a + a
				in h 3
				ni
		in g 3
		ni
 in f 5
 ni
