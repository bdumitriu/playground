module Nsort where

msort :: Ord a => [a] -> [a]
msort = pairwise [] . ascending

pairwise :: Ord a => [[a]] -> [[a]] -> [a]
pairwise []  []            = []
pairwise []  [xs]          = xs
pairwise yss (xs1:xs2:xss) = pairwise ((xs1 `merge` xs2) : yss) xss
pairwise yss xss           = pairwise [] (xss ++ reverse yss)

merge :: Ord a => [a] -> [a] -> [a]
merge []     []     = []
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys)
  | x <= y          = x : merge xs (y:ys)
  | otherwise       = y : merge (x:xs) ys

ascending :: Ord a => [a] -> [[a]]
ascending [] = []
ascending xs = ys : ascending zs
 where
  (ys,zs) =
    case ascends (<=) xs of
      ([_],_) -> let (x,y) = ascends (>=) xs in (reverse x,y)
      yszs    -> yszs

ascends :: (a -> a -> Bool) -> [a] -> ([a],[a])
ascends prec (x:y:xs)
  | prec x y  = let (ys,zs) = ascends prec (y:xs) in (x:ys,zs) 
  | otherwise = ([x], y:xs)
ascends _ xs  = (xs,[])
