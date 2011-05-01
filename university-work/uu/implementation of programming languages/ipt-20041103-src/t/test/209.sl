let a :: Int = 5
 ;  f :: Int -> Int =
          \x -> let g :: Int -> Int =
                          \y -> let h :: Int -> Int = \z -> a
                                in  h 3
                                ni
                in  g 3
                ni
in f 5
ni
