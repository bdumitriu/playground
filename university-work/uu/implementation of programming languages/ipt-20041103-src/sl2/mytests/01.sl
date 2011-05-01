--
-- The basic test which is provided in the book. Since there is only
-- one cross-level access to a variable from level 0 (a) from code
-- at level 3 (body of h), no static link is precomputed.
--

let a :: Int = 5
 ;  f :: Int -> Int =
	\x -> let g :: Int -> Int =
			\y -> let h :: Int -> Int = \z -> a
				in h 3
				ni
		in g 3
		ni
 in f 5
 ni
