let factor :: Int = 3
  ; calc   :: Int -> Int =
               \x -> if x > 0 then factor * calc (x-1) else 1 fi
 in calc 4
 ni
