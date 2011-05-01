module Msort where

-- toplevel function

msort :: Ord a => [a] -> [a]
msort = pairwise [] . units

-- pairwise merging

pairwise :: Ord a => [[a]] -> [[a]] -> [a]
pairwise []  []            = []
pairwise []  [xs]          = xs
pairwise yss (xs1:xs2:xss) = pairwise ((xs1 `merge` xs2) : yss) xss
pairwise yss xss           = pairwise [] (xss ++ reverse yss)

-- merging two sorted lists

merge :: Ord a => [a] -> [a] -> [a]
merge []     []     = []
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) ys

-- divide into unit lists

units :: [a] -> [[a]]
units xs = map (:[]) xs
