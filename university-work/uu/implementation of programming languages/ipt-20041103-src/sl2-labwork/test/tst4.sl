let a :: Int = 5
  ; id :: Int -> Int = \x -> x
  ; const :: Int -> Int -> Int = \x y -> x
  ; f :: Int -> Int =
  		\b -> let g :: Int -> Int -> Int =
			\p q -> id a * const q p + b
			in g 3 4
			ni
 in f 7
 ni