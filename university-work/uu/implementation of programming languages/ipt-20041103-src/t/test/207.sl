let v1 :: Int = 4
 ;  f1 :: Int -> Int = \x -> x + v1
 ;  f2 :: (Int -> Int) -> Int -> Int =
          \f y -> let ff2 :: Int -> Int =
                             \y -> f y + f1 y
                   in ff2 (2 * y)
                   ni
 in f2 f1 3
 ni