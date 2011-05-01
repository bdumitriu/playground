{- faulty: compiler accepts, but in machine 'one' refers to multiply used location.
   Compiler assumes it is only used for 'one'.
 -}
let incr :: Int -> Int
    = \x -> let one :: Int = 1
             in x+one
             ni
 in (incr 3) + (incr 5)
 ni