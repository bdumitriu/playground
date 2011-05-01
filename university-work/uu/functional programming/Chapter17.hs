{-
	Bogdan Dumitriu
-}

type Vector = [Float]

-- Exercise 17.2, page 350

--partes []     = [[]]
--partes (x:xs) = (partes xs) ++ (map (x: ) (partes xs))

--partes []     = [[]]
--partes (x:xs) = u ++ (map (x: )u)
--    where
--    u = partes xs

partes = foldl j [[]]
    where
    j u x = u ++ (map (x:) u)

subLists :: [a] -> [[a]]
subLists []	= [[]]
subLists (x:xs)	= concat [ [x:el, el] | el <- subLists xs]

subSeq :: [a] -> [[a]]
subSeq []	= [[]]
subSeq (x:xs)	= [take i (x:xs) | i <- [1 .. length (x:xs)]] ++ subSeq xs

taket :: Int -> Int -> [a] -> [a]
taket i j = take (j-i+1) . drop (i-1)

subSeq2 :: [a] -> [[a]]
subSeq2 l = [taket i j l | i <- [1..len], j <- [i..len]]
  where
  len = length l

-- Exercise 17.4, page 350

scalarProduct :: Vector -> Vector -> Float
scalarProduct = curry $ sum . (uncurry $ zipWith (*))

--scalarProduct :: Vector -> Vector -> Float
--scalarProduct xs = sum . zipWith (*) xs

-- Exercise 17.8, page 350

--[m*m | m <- [1 .. 10]]
f1 = map (^2) [1 .. 10]

-- [m*m | m <- [1 .. 10], m*m < 50]
f2 = map (^2) . filter (\x -> x*x < 50) $ [1 .. 10]

-- [x+y | x <- [1 .. 4], y <- [2 .. 4], x > y]
f3 = concat (map (\x -> map (x+) (filter (x>) [2 .. 4])) [1 .. 4])

perms :: [a] -> [[a]]
perms []	= [[]]
perms (x:xs)	= [before ++ [x] ++ after | y <- pxs, i <- [0 .. length y], (before, after) <- [splitAt i y]]
  where
  pxs			= perms xs

-- [x:p | x <- xs, p <- perms (xs\\[x])]
-- ???

-- Exercise 17.22, page 369

fact 0 = 1
fact n = n * fact (n-1)

factorial2 = [fact n | n <- [0 .. ]]

fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

fibonacci2 = [fibo n | n <- [0 ..]]

-- Exercise 17.23, page 369

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

pfactors :: Int -> [Int]
pfactors n = [x | x <- factors n, memberOrd primes x]

sieve :: [Int] -> [Int]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]

primes :: [Int]
primes = sieve [2 .. ]

memberOrd :: Ord a => [a] -> a -> Bool
memberOrd (x:xs) n
  | x < n	= memberOrd xs n
  | x == n	= True
  | otherwise	= False

hamming :: [Int]
hamming = [x | x <- [1 .. ], ham . pfactors $ x]


ham :: [Int] -> Bool
ham []		= True
ham [2]		= True
ham [3]		= True
ham [5] 	= True
ham [2, 3]	= True
ham [2, 5]	= True
ham [3, 5]	= True
ham [2, 3, 5]	= True
ham _		= False

-- Exercise 17.24, page 370

runningSums :: [Int] -> [Int]
runningSums list = 0 : zipWith (+) list (runningSums list)

-- Exercise 17.25, page 370

infProd :: [a] -> [b] -> [(a, b)]
infProd xs ys = [(lx, y) |  y <- init ys] ++ [(x, ly) | x <- xs]
  where
  lx = last xs
  ly = last ys

infiniteProduct :: [a] -> [b] -> [(a, b)]
infiniteProduct xs ys = concat [infProd (take i xs) (take i ys) | i <- [1 .. ]]

-- Exercise 17.26, page 373
-- [2^n | n <- [0 .. ]]

powers2 = scanl (*) 1 [2, 2 .. ]

powersn n = scanl (*) 1 [n, n .. ]

powers3 = powersn 3
powers4 = powersn 4
powers5 = powersn 5

--powers2 = out
--  where
--  out = 1 : zipWith (*) [2, 2 ..] out

-- Exercise 17.27, page 373

runningSumsPos :: [Int] -> [Int]
runningSumsPos = scanl (+) 0 . filter (>0)

-- Exercise 17.28, page 373

merge :: Ord a => [a] -> [a] -> [a]
merge [] x		= x
merge x []		= x
merge (x:xs) (y:ys)	= if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []		= []
removeDuplicates (x:y:xs)	= if x == y then removeDuplicates (y:xs) else x : removeDuplicates (y:xs)

-- Exercise 17.29, page 373

factorial = 1 : zipWith (*) [1 .. ] factorial

fibonacci = 1 : 1 : zipWith (+) (tail fibonacci) fibonacci

