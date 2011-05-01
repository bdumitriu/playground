{-
	Bogdan Dumitriu
-}

-- Exercise 13.1, page 237

--f n	= 37 + n
--f True	= 34

--g 0	= 37
--g n	= True

--h x
--  | x > 0	= True
--  | otherwise	= 37

--k x	= 34
--k 0	= 35


--f :: Num b => [a] -> [b] -> a -> b
--f x y z = 5

--h x = f x x

-- Exercise 13.7, page 238

-- load and just say ":t fun y"

y :: (Int, a)
y = y

fun :: (a, Char) -> (a, Char)
fun = fun

