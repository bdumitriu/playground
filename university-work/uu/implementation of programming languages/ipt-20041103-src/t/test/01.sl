let y  :: Int = 1
 ;  f1 :: Int -> Int = \x -> x + y
 ;  f2 :: (Int -> Int) -> Int -> Int
        = \f y -> let ff2 :: Int -> Int = \y -> f1 y
                   in if y > 0
                      then f2 ff2 (y-1)
                      else f 1
                      fi
                   ni
 in f2 f1 3
 ni