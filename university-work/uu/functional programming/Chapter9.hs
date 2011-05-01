{-
	Bogdan Dumitriu
-}

import Prelude hiding (takeWhile,getLine)

-- Exercise 9.2, page 159

mylength :: [a] -> Int
mylength list = sum (map (\x -> 1) list)

-- Exercise 9.3, page 159

addUp ns = filter greaterOne (map addOne ns)

greaterOne n = n > 1

addOne n = n + 1

addUp2 ns = map addOne (filter greaterZero ns)

greaterZero n = n > 0

-- Exercise 9.4, page 160

testMap ns = map addOne (map addOne ns)

testMap2 ns = map (addOne . addOne) ns

-- Exercise 9.5, page 160

testFil ns = filter greaterOne (filter lessTen ns)

lessTen n = n < 10

testFil2 ns = filter (\x -> greaterOne x && lessTen x) ns

-- Exercise 9.6, page 160

squares ns = map (\x -> x^2) ns

sumsq ns = sum (squares ns)

allPos ns = length (filter (\x -> x <= 0) ns) == 0

-- Exercise 9.7, page 160

minList :: (Ord a) => [a] -> a
minList []	= error "minList not defined on empty list"
minList [x]	= x
minList (x:xs)	= min x (minList xs)

minFun :: (Ord a) => (Int -> a) -> Int-> a
minFun f n = minList (map f [0 .. n])

testFun :: (Eq a) => (Int -> a) -> Int -> Bool
testFun f n	= testFunAux f n (f 0)

testFunAux :: (Eq a) => (Int -> a) -> Int -> a -> Bool
testFunAux f n firstVal = length (filter (\x -> f x /= firstVal) [1 .. n]) == 0

testFunGZ :: (Int -> Int) -> Int -> Bool
testFunGZ f n = length (filter (\x -> x <= 0) (map f [0 .. n])) == 0

testFunInc :: (Int -> Int) -> Int -> Bool
testFunInc f n = length (filter (\x -> f x >= f (x + 1)) [0 .. n-1]) == 0

-- Exercise 9.8, page 160
twice :: (a -> a) -> a -> a
twice f n = (f . f) n

-- Exercise 9.9, page 160

iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

-- Exercise 9.10, page 161

twoton :: Int -> Int
twoton n = iter n (\x -> 2*x) 1

-- Exercise 9.11, page 163

sumsq2 :: Int -> Int
sumsq2 n = foldr (+) 0 (map (\x -> x^2) [0 .. n])

-- Exercise 9.12, page 163

sumsqpos ns = foldr (+) 0 (map (\x -> x^2) (filter (\x -> x > 0) ns))

-- Exercise 9.13, page 163

myunzip :: [(a,b)] -> ([a],[b])
myunzip list = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([],[]) list

mylast :: [a] -> a
mylast list = foldr1 (\x y -> y) list

myinit :: [a] -> [a]
myinit []	= error "myinit cannot be applied to the empty list"
myinit s	= fst (foldr (\x (xs, t) -> if t == True then ([], False) else (x:xs, False)) ([], True) s)

-- yet another my init (yami)
yami :: [a] -> [a]
yami s = tail . foldr f (\x -> []) s $ undefined
  where
  f x r = (\arg -> arg : r x)

-- Exercise 9.14, page 164

mystery xs = foldr (++) [] (map (\x -> [x]) xs)

-- Exercise 9.15, page 164

type Word = String
type Line = [Word]

formatList :: (a -> String) -> [a] -> String
formatList f list = foldr (++) "" (map f list)

formatLine :: Line -> String
formatLine [] = "\n"
formatLine line = foldr (++) "" ((head line) : (map (" " ++) (tail line))) ++ "\n"

formatLines :: [Line] -> String
formatLines lines = formatList formatLine lines

-- Exercise 9.16, page 164

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ []	= []
filterFirst p (x:xs)
  | p x			= x : filterFirst p xs
  | otherwise		= xs

-- Exercise 9.17, page 164

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p list = fst (foldr (\x (lst,test) -> if (test == True) && (not (p x)) then (lst,False) else (x:lst,test)) ([],True) list)

-- Exercise 9.19, page 166

getUntil :: (a -> Bool) -> [a] -> [a]
getUntil _ []	= []
getUntil p (x:xs)
  | p x		= []
  | otherwise	= x : getUntil p xs

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ []	= []
dropUntil p (x:xs)
  | p x		= x : xs
  | otherwise	= dropUntil p xs

-- Exercise 9.20, page 166

dropSpace :: String -> String
dropSpace st = dropUntil (\x -> not (elem x "\t\n ")) st

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p list = getUntil (not . p) list

-- Exercise 9.21, page 166

mytail :: [a] -> [a]
mytail [] = []
mytail xs = tail xs

splitString :: String -> [String]
splitString ""	= []
splitString st	= getUntil f st : splitString (mytail (dropUntil f st))
  where
  f x = x == '\n'

-- Exercise 9.23, page 166

lineLen = 35

getLine :: (a -> Bool) -> [a] -> [a]
getLine _ []	= []
getLine p (x:xs)
  | p x		= x : getLine p xs
  | otherwise	= []

dropLine :: (a -> Bool) -> [a] -> [a]
dropLine _ []	= []
dropLine p (x:xs)
  | p x		= x : xs
  | otherwise	= dropLine p xs

splitLines :: (a -> Bool) -> (a -> Bool) -> [a] -> [[a]]
splitLines _ _ []	= []
splitLines p q line	= getLine p line : splitLines p q (dropLine q line)
