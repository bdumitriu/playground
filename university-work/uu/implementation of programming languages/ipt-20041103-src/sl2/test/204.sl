{- faulty: partially parameterization (of sum) is allowed (by the compiler)
   but machine cannot handle it
 -}
let apply  :: (Int -> Int) -> Int -> Int = \f x -> f x
  ; sum    :: Int -> Int -> Int          = \x y -> x + y
  ; incBy1 :: Int -> Int                 = sum 1
 in apply incBy1 3
 ni
