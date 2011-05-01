let apply :: (Int -> Int) -> Int -> Int =
             \f x -> f x
 ;  incBy :: Int -> (Int -> Int) =
             \x -> let f :: Int -> Int = \y -> x + y
                    in f
                    ni
 in apply (incBy 1) 3
 ni
