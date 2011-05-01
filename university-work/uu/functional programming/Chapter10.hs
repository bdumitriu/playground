{-
	Bogdan Dumitriu
-}

import Pictures hiding (combine)
import Prelude hiding (flip, lines)

-- Exercise 10.3, page 171

composeList :: [a -> a] -> (a -> a)
composeList [] = id
composeList (x:xs) = x . composeList xs

-- Exercise 10.5, page 174

iter :: Int -> (a -> a) -> (a -> a)
iter n f = foldr (.) id (replicate n f)

succn :: Int -> (Int -> Int)
succn n = iter n succ

-- Exercise 10.6, page 174

zipSwitched :: (a -> b -> c) -> [b] -> [a] -> [c]
zipSwitched f = zipWith (\a b -> f b a)

-- Exercise 10.7, page 175

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = (\a b -> f b a)

-- Exercise 10.8, page 175

fn108 :: Char -> Bool
fn108 = (\x -> (not . elem x) "\n\t ")

-- Exercise 10.9, page 175

total :: (Int -> Int) -> (Int -> Int)
total f = (\n -> foldr (+) 0 (map f [0 .. n]))

-- Exercise 10.10, page 175

f x = x^2

slope :: (Float -> Float) -> (Float -> Float)
slope f = (\x -> (f (x+0.00001) - f x) / 0.00001)

-- Exercise 10.11, page 175

precision = 0.00001

integrate :: (Float -> Float) -> (Float -> Float -> Float)
integrate f = (\x1 x2 -> sum (map (\x -> f x * precision) [x1+precision, x1+2*precision .. x2]))

-- Exercise 10.12, page 180

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g x = ((g . f) x) . f

totalPA :: (Int -> Int) -> (Int -> Int)
totalPA f = foldr (+) 0 . list0n

list0n n = [0 .. n]

-- Exercise 10.13, page 180

fn1013 :: [Int] -> [Int]
fn1013 = map (+1) . filter (>=0)

fnref :: [Int] -> [Int]
fnref = filter (>0) . map (+1)

-- Exercise 10.14, page 183

chessBoard :: Int -> Picture
chessBoard n
  | n == 0		= []
  | n `mod` 2 == 0	= foldr1 above (concat (replicate (n `div` 2) [clw, clb]))
  | otherwise		= above (foldr1 above (concat (replicate (n `div` 2) [clw, clb]))) clw
  where
  clw = chessLineWhite n
  clb = chessLineBlack n

chessLineWhite :: Int -> Picture
chessLineWhite n
  | n `mod` 2 == 0	= foldr1 sideBySide (concat (replicate (n `div` 2) [white, black]))
  | otherwise		= sideBySide (foldr1 sideBySide (concat (replicate (n `div` 2) [white, black]))) white

chessLineBlack :: Int -> Picture
chessLineBlack n
  | n `mod` 2 == 0	= foldr1 sideBySide (concat (replicate (n `div` 2) [black, white]))
  | otherwise		= sideBySide (foldr1 sideBySide (concat (replicate (n `div` 2) [black, white]))) black

bwLine :: Int -> Picture
bwLine = foldr1 sideBySide . myrep blackWhite

myrep :: a -> Int -> [a]
myrep x y = replicate y x

whiteBlack = sideBySide white black

blackWhite = sideBySide black white

-- Exercise 10.16, page 183

makePicture :: Int -> Int -> [(Int, Int)] -> Picture
makePicture m n blacks = [[makePoint i j blacks | j <- [0 .. n-1]] | i <- [0 .. m-1]]

makePoint :: Int -> Int -> [(Int, Int)] -> Char
makePoint x y blacks = if elem (x, y) blacks then '#' else '.'

makePic (x,y,z) = makePicture x y z

-- just a test...
makeMat :: Int -> Int -> [[(Int, Int)]]
makeMat m n = map (\i -> map (\j -> (i, j)) [0 .. n-1]) [0 .. m-1]

-- Exercise 10.17, page 183

pictureToRep :: Picture -> (Int, Int, [(Int, Int)])
pictureToRep p = (length p, (length . head) p, makeBlackList (addIndices2 p))

makeBlackList :: [[(Int, Int, Char)]] -> [(Int, Int)]
makeBlackList = map (\(x, y, _) -> (x, y)) . filter (\(_, _, ch) -> ch == '#') . concat

addIndices2 :: [[a]] -> [[(Int,Int,a)]]
addIndices2 list = zipWith (\i lst -> map (\(j,val) -> (i,j,val)) lst) [0 .. len-1] (map addIndices list)
  where
  len = length list

addIndices :: [a] -> [(Int, a)]
addIndices list = zipWith (\x y -> (x, y)) [0 .. len-1] list
  where
  len = length list

-- creating an index for a document

type Doc = String
type Line = String
type Word = String

makeIndex :: Doc -> [([Int], Word)]
makeIndex = 
	shorten		.	-- [([Int], Word)]	-> [([Int], Word)]
	altAmalgamate	.	-- [([Int], Word)]	-> [([Int], Word)]
	makeLists	.	-- [(Int, Word)]	-> [([Int], Word)]
	sortLs		.	-- [(Int, Word)]	-> [(Int, Word)]
	allNumWords	.	-- [(Int, Line)]	-> [(Int, Word)]
	numLines	.	-- [Line]		-> [(Int, Line)]
	lines			-- Doc			-> [Line]

-- Exercise 10.20, page 191

lines :: Doc -> [Line]
lines ""	= []
lines s		= takeWhile p s : lines (if null restS then [] else tail restS)
  where
  p x = x /= '\n'
  restS = dropWhile p s

numLines :: [Line] -> [(Int, Line)]
numLines = zip [1 .. ]

seps :: String
seps = " \n\t;:.,\"!?()-"

getWord :: Line -> Word
getWord []	= []
getWord (ch:s)
  | elem ch seps	= []
  | otherwise		= ch : getWord s

dropWord :: Line -> Line
dropWord []	= []
dropWord (ch:s)
  | elem ch seps	= ch:s
  | otherwise		= dropWord s

dropSeps :: Line -> Line
dropSeps []	= []
dropSeps (ch:s)
  | elem ch seps	= dropSeps s
  | otherwise		= ch:s

sw :: Line -> [Word]
sw []	= []
sw line	= getWord line : sw (dropSeps (dropWord line))

splitWords :: Line -> [Word]
splitWords = sw . dropSeps

numWords :: (Int, Line) -> [(Int, Word)]
numWords (_, []) = []
numWords (ln, line) = map (\w -> (ln, w)) (splitWords line)

allNumWords :: [(Int, Line)] -> [(Int, Word)]
allNumWords = concat . map numWords

orderPair :: (Int, Word) -> (Int, Word) -> Bool
orderPair (i1, w1) (i2, w2) = w1 < w2 || (w1 == w2 && i1 < i2)

sortLs :: [(Int, Word)] -> [(Int, Word)]
sortLs []	= []
sortLs (x:xs)	= sortLs [el | el <- xs, orderPair el x] ++  [x] ++ sortLs [el | el <- xs, orderPair x el]

makeLists :: [(Int, Word)] -> [([Int], Word)]
makeLists = map (\(x, y) -> ([x], y))

amalgamate :: [([Int], Word)] -> [([Int], Word)]
amalgamate []	= []
amalgamate [p]	= [p]
amalgamate ((l1, w1) : (l2, w2) : rest)
  | w1 == w2	= amalgamate ((l1 ++ l2, w2) : rest)
  | otherwise	= (l1, w1) : amalgamate ((l2, w2) : rest)

shorten :: [([Int], Word)] -> [([Int], Word)]
shorten = filter (\(_, w) -> length w > 3)

-- Exercise 10.22, page 191

makeModIndex :: Doc -> [([(Int, Int)], Word)]
makeModIndex = 
	collapse	.	-- [([Int], Word)]	-> [([(Int, Int)], Word)]
	shorten		.	-- [([Int], Word)]	-> [([Int], Word)]
	amalgamate	.	-- [([Int], Word)]	-> [([Int], Word)]
	makeLists	.	-- [(Int, Word)]	-> [([Int], Word)]
	sortLs		.	-- [(Int, Word)]	-> [(Int, Word)]
	allNumWords	.	-- [(Int, Line)]	-> [(Int, Word)]
	numLines	.	-- [Line]		-> [(Int, Line)]
	lines			-- Doc			-> [Line]

collapse :: [([Int], Word)] -> [([(Int, Int)], Word)]
collapse = map collapseWord

collapseWord :: ([Int], Word) -> ([(Int, Int)], Word)
collapseWord (x, y) = (collapseN x, y)

collapseN :: [Int] -> [(Int, Int)]
collapseN []		= []
collapseN [x]		= [(x, x)]
collapseN (x:xs)
  | x + 1 == x1	=  (x, x2) : rest
  | otherwise	= (x, x) : (x1, x2) : rest
  where
  ((x1, x2):rest) = collapseN xs

-- Exercise 10.23, page 192

altSortLs :: [(Int, Word)] -> [(Int, Word)]
altSortLs []		= []
altSortLs (x:xs)	= altSortLs [el | el <- xs, orderPair el x] ++  [x] ++ altSortLs [el | el <- xs, not (orderPair el x)]

-- Exercise 10.24, page 192

altAmalgamate :: [([Int], Word)] -> [([Int], Word)]
altAmalgamate []		= []
altAmalgamate ((l, w):s)	= (combine . getUntil p $ ((l, w):s)) : (altAmalgamate . dropUntil p $ ((l, w):s))
  where
  p (_,arg) = arg /= w

combine :: [([Int], Word)] -> ([Int], Word)
combine []	= error "you cannot combine an empty list"
combine list	= ((map (\([x], y) -> x) list), snd . head $ list)

getUntil :: (a -> Bool) -> [a] -> [a]
getUntil p = takeWhile (\x -> not (p x))

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p = dropWhile (\x -> not (p x))

-- Exercise 10.25, page 192

sizer :: (a, [b]) -> Bool
sizer = (>3) . length . snd

-- Exercise 10.27, page 192

printIndex :: [([Int], Word)] -> IO()
printIndex = putStr . writeIndex

writeIndex :: [([Int], Word)] -> String
writeIndex []	= ""
writeIndex list	= concat . map writeEntry $ (pad list (getPadding list + 5))

writeEntry :: ([Int], Word) -> String
writeEntry (list, word) = word ++ writeList list ++ "\n"

writeList :: [Int] -> String
writeList []		= []
writeList [x]		= show x
writeList (x:xs)	= show x ++ ", " ++ (writeList xs)

getPadding :: [([Int], Word)] -> Int
getPadding = maxList . map (\(_, word) -> length word)

maxList :: Ord a => [a] -> a
maxList []	= error "cannot apply maxList to empty list"
maxList [x]	= x
maxList (x:xs)	= x `max` (maxList xs)

pad :: [([Int], Word)] -> Int -> [([Int], Word)]
pad list nr = map (\x -> padWord x nr) list

padWord :: ([Int], Word) -> Int -> ([Int], Word)
padWord (l, word) nr
  | len < nr	= (l, word ++ replicate (nr - len) ' ')
  | otherwise	= (l, word)
  where
  len = length word
