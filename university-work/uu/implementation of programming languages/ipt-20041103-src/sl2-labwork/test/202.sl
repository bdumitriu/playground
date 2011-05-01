let apply :: (Int -> Int) -> Int -> Int =
              \f x -> f x
  ; incBy1 :: Int -> Int = \x -> x+1
 in apply incBy1 2
 ni
