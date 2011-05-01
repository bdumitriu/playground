{-
	Bogdan Dumitriu
-}

import Data.List hiding (sort)

-- Section 5.2

goo1 s = (s, length s)

goo2 s = length s : s

goo3 s t = [s] : t

goo4 s t = take (length s) t

goo5 (x:s) (y:t)
  | length x >= length y	= x : goo5 s t
  | otherwise			= y : goo5 s t
goo5 _ _			= []

--goo6 op (x:s) (y:t)	= op x y : goo6 s t
--goo6 _ _ _		= []

goo7 s = map f s
  where
  f x = [(x, x)]

goo8 s = filter p s
  where
  p (x, y) = x

--goo9 s = filter p s
--  where
--  p (x, y) = x:y

goo10 s = concat (map f s)
  where
  f (x, y) = take x y

-- Section 5.3

quicksort :: Ord a => [a] -> [a]
quicksort []		= []
quicksort (x:xs)	= [el | el <- xs, el <= x] ++ x : [el | el <- xs, el > x]

sort :: Ord a => [a] -> [a]
sort = quicksort

hoo1 s t = head s `elem` head t

--hoo2 s t = if s < t then s:t else t:s

hoo3 s t = if s < t then [s, t] else [t, s]

hoo4 []		= []
hoo4 ((x, _):s)	= sort x : hoo4 s

hoo5 x y = x + head y

hoo6 x y = x + length y

hoo7 x y
  | x == y	= x + 1
  | otherwise	= 0

hoo8 x y
  | x > y	= x + 1
  | otherwise	= 0

--hoo9 = [] `elem` [[]]

hoo10 x y = filter p y
  where
  p (a, _) = a == x

perms [] = [[]]
perms xs = [x:ps | x <- xs, ps <- perms (xs\\[x])]