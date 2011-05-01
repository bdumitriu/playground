let a :: Int = 5
 ;  f :: Int -> Int =
	\x -> let b :: Int = 6
	       ;  g :: Int -> Int =
			\y -> let c :: Int = 100
				; h :: Int -> Int =
					\z -> let i :: Int -> Int = \zz -> zz -- + a + b + a + b + b + c + c
						in a := 1 ; b := 2 ; c := 3; 100*c + 10*b + 1*c
						ni
				in h 3
				ni
		in g 3
		ni
 in f 5
 ni
