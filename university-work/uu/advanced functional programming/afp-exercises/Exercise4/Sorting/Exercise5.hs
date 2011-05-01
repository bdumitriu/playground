import Nsort
import Test.QuickCheck
import List

-- try more test cases with a custom check function

myConfig = defaultConfig { configMaxFail = 20000 }

myCheck x = check myConfig x

-- the properties for Exercise 5

prop_msort :: [Int] -> Property
prop_msort xs =
  length xs > 1 ==>
    collect (length xs) $
      sorted (msort xs) && msort xs `sameElems` xs

prop_pairwise :: [[Int]] -> [[Int]] -> Property
prop_pairwise xs ys = 
  and (map sorted xs) && and (map sorted ys) && length ys > 1 ==>
    collect (length xs, length ys) $
      sorted result && (concat xs ++ concat ys) `sameElems` result
  where result = pairwise xs ys

prop_merge :: [Int] -> [Int] -> Property
prop_merge xs ys =
  sorted xs && sorted ys && length xs > 1 && length ys > 1 ==>
    collect (length xs, length ys) $
      sameElems (xs ++ ys) xsys && sorted xsys
  where xsys = merge xs ys

prop_ascending :: [Int] -> Property
prop_ascending xs =
  length xs > 1 ==>
    collect (length xs) $
      and (map sorted asc) && xs `sameElems` (concat asc)
  where asc = ascending xs

-- helper functions

sorted :: Ord a => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _        = True

sameElems :: Eq a => [a] -> [a] -> Bool
[] `sameElems` []       = True
(x:xs) `sameElems` ys   = (x `elem` ys) && (xs `sameElems` (ys \\ [x]))
_ `sameElems` _         = False
