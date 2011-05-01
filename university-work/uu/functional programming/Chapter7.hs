{-
	Bogdan Dumitriu
-}

import Prelude hiding (drop, splitAt, seq, sequence, product, and, or, reverse, unzip, zip3, getLine)
import Char

-- Exercise 7.1, page 119

plusone1 :: [Int] -> Int
plusone1 list = case list of
  [] -> 0
  (x:_) -> x + 1

plusone2 :: [Int] -> Int
plusone2 [] = 0
plusone2 (x:_) = x + 1

-- Exercise 7.2, page 119

whatever :: [Int] -> Int
whatever [] = 0
whatever [x] = x
whatever (x:y:s) = x + y

-- Exercise 7.4, page 120

product :: [Int] -> Int
product []	= 1
product (x:xs)	= x * product xs

-- Exercise 7.5, page 120

and :: [Bool] -> Bool
and []		= True
and (x:xs)	= x && and xs

or :: [Bool] -> Bool
or []		= False
or (x:xs)	= x || or xs

-- Exercise 7.6, page 125

elemNum1 :: Int -> [Int] -> Int
elemNum1 _ [] 		= 0
elemNum1 x (y:xs)
  | (x == y)		= 1 + elemNum1 x xs
  | otherwise		= elemNum1 x xs

elemNum2 :: Int -> [Int] -> Int
elemNum2 x list = length [elem | elem <- list, elem == x]

-- Exercise 7.7, page 125

unique1 :: [Int] -> [Int]
unique1 list = [elem | elem <- list, elemNum2 elem list == 1]

-- a quicksort on a list of type [(a,Int)], with the elements of type a being sorted
extQuicksort :: (Ord a) => [(a,Int)] -> [(a,Int)]
extQuicksort []			= []
extQuicksort ((x,posx):xs)	= extQuicksort [el | el <- xs, fst el < x] ++ [(x,posx)] ++ extQuicksort [el | el <- xs, fst el >= x]

-- should be called with <first_index> as the first argumet
-- transforms [1,2,3] to [(1,<first_index>),(2,<first_index>+1),(3,<first_index>+2)]
-- if, for example, <first_index> is 0, then [1,2,3] will become [(1,0),(2,1),(3,2)]
addIndex :: Int -> [a] -> [(a,Int)]
addIndex n []		= []
addIndex n (x:xs)	= (x,n) : addIndex (n + 1) xs

-- helper function for the main purge function
purgeAux :: (Eq a) => a -> [(a,Int)] -> [(a,Int)]
purgeAux n []		= []
purgeAux n [x]
  | (fst x == n)	= []
  | otherwise		= [x]
purgeAux n (x:y:xs)
  | (fst x == fst y)	= purgeAux (fst x) xs
  | (fst x == n)	= purgeAux (fst x) (y:xs)
  | otherwise		= x : purgeAux (fst x) (y:xs)

-- from a sorted list of type [(a,Int)] takes away all the elements which appear more than once
purge :: (Eq a) => [(a,Int)] -> [(a,Int)]
purge []		= []
purge [x]		= [x]
purge (x:y:xs)
  | (fst x == fst y)	= purgeAux (fst x) xs
  | otherwise		= x : (purgeAux (fst x) (y:xs))

-- returns the list containing only the second element in each pair (the Int element) from the first list
getSecond :: [(a,Int)] -> [Int]
getSecond list = [el | (_,el) <- list]

-- basic quicksort on a list
quicksort :: (Ord a) => [a] -> [a]
quicksort []		= []
quicksort (x:xs)	= quicksort [el | el <- xs, el < x] ++ [x] ++ quicksort [el | el <- xs, el >= x]

-- returns the list of elements from the first list found on the positions given by the elements of
-- the second list. Indices in second list should be 0 based
returnWithPos :: [a] -> [Int] -> [a]
returnWithPos _ []		= []
returnWithPos list (x:xs)	= list!!x : returnWithPos list xs

unique2 :: (Ord a) => [a] -> [a]
unique2 list = returnWithPos list positions
  where
  positions = quicksort (getSecond (purge (extQuicksort (addIndex 0 list))))

-- Exercise 7.8, page 125

reverse :: [a] -> [a]
reverse []	= []
reverse (x:xs)	= reverse xs ++ [x]

rev :: [a] -> ([a] -> [a])
rev []		= (\x -> x)
rev (y:ys)	= (\x -> (rev ys) (y:x))

reverse2 :: [a] -> [a]
reverse2 x = rev x []

unzip :: [(a,b)] -> ([a],[b])
unzip []		= ([],[])
unzip ((x,y):tail)	= (x:(fst unzippedTail),y:(snd unzippedTail))
  where
  unzippedTail = unzip tail

-- Exercise 7.9, page 125

minList :: (Ord a) => [a] -> a
minList []	= error "minList undefined for empty list"
minList [x]	= x
minList (x:xs)	= min x (minList xs)

maxList :: (Ord a) => [a] -> a
maxList []	= error "maxList undefined for empty list"
maxList [x]	= x
maxList (x:xs)	= max x (maxList xs)

-- Exercise 7.11, page 125

insAscending :: (Ord a) => a -> [a] -> [a]
insAscending x []	= [x]
insAscending x (y:ys)
  | x <= y		= x:y:ys
  | otherwise		= y:(insAscending x ys)

insDescending :: (Ord a) => a -> [a] -> [a]
insDescending x []	= [x]
insDescending x (y:ys)
  | x >= y		= x:y:ys
  | otherwise		= y:(insDescending x ys)

insDropDuplicates :: (Ord a) => a -> [a] -> [a]
insDropDuplicates x []	= [x]
insDropDuplicates x (y:ys)
  | x < y		= x:y:ys
  | x == y		= x:ys
  | otherwise		= y:(insDropDuplicates x ys)

-- just a wrapper function so that iSort doesn't have to be changed
ins :: (Ord a) => a -> [a] -> [a]
ins x xs = insDropDuplicates x xs

iSort :: (Ord a) => [a] -> [a]
iSort []	= []
iSort (x:xs)	= ins x (iSort xs)

-- Exercise 7.13, page 125

insPairs :: (Ord a) => (a,a) -> [(a,a)] -> [(a,a)]
insPairs x []			= [x]
insPairs (x1,x2) ((y1,y2):ys)
  | x1 < y1			= (x1,x2):(y1,y2):ys
  | (x1 == y1) && (x2 <= y2)	= (x1,x2):(y1,y2):ys
  | otherwise			= (y1,y2):(insPairs (x1,x2) ys)

iSortPairs :: (Ord a) => [(a,a)] -> [(a,a)]
iSortPairs []		= []
iSortPairs (x:xs)	= insPairs x (iSortPairs xs)

-- Exercise 7.14, page 128

drop :: Int -> [a] -> [a]
drop _ []	= []
drop 0 xs	= xs
drop n (x:xs)	= drop (n - 1) xs

splitAt :: Int -> [a] -> ([a],[a])
splitAt _ []		= ([],[])
splitAt n xs | n <= 0	= ([],xs)
splitAt n (x:xs)	= let (fp,sp) = splitAt (n-1) xs in (x:fp,sp)

-- Exercise 7.15, page 128
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 [] _ _			= []
zip3 _ [] _			= []
zip3 _ _ []			= []
zip3 (x:xs) (y:ys) (z:zs)	= (x,y,z) : (zip3 xs ys zs)

zip3Alt :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3Alt xs ys zs = zipWith (\x (y,z) -> (x,y,z)) xs (zip ys zs)

-- Exercise 7.18, page 128

qSortDesc :: (Ord a) => [a] -> [a]
qSortDesc []		= []
qSortDesc (x:xs)	= qSortDesc [el | el <- xs, el >= x] ++ [x] ++ qSortDesc [el | el <- xs, el < x]

qSortDelDup :: (Ord a) => [a] -> [a]
qSortDelDup []		= []
qSortDelDup (x:xs)	= qSortDelDup [el | el <- xs, el < x] ++ [x] ++ qSortDelDup [el | el <- xs, el > x]

-- Exercise 7.18, page 128

sublist :: [Char] -> [Char] -> Bool
sublist [] _		= True
sublist _ []		= False
sublist (x:xs) (y:ys)
  | x == y		= sublist xs ys
  | otherwise		= sublist (x:xs) ys

seq :: Int -> [Char] -> [Char] -> Bool
seq _ [] _	= True
seq _ _ []	= False
seq n xs (y:ys)	= ((take n (y:ys)) == xs) || (seq n xs ys)

sequence :: [Char] -> [Char] -> Bool
sequence x y = seq (length x) x y

-- <definitions-from-book>
type Word = String
type Line = [Word]

whitespace = ['\n','\t',' ']

getWord :: String -> String
getWord []		= []
getWord (x:xs)
  | elem x whitespace	= []
  | otherwise		= x : getWord xs

dropWord :: String -> String
dropWord []		= []
dropWord (x:xs)
  | elem x whitespace	= x : xs
  | otherwise		= dropWord xs

dropSpace :: String -> String
dropSpace []		= []
dropSpace (x:xs)
  | elem x whitespace	= dropSpace xs
  | otherwise		= x : xs

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split []	= []
split st	= (getWord st) : split (dropSpace (dropWord st))

getLine :: Int -> [Word] -> Line
getLine len []	= []
getLine len (w:ws)
  | len >= wLen	= w : getLine (len - wLen - 1) ws
  | otherwise	= []
  where wLen = length w

dropLine :: Int -> [Word] -> Line
dropLine len []	= []
dropLine len (w:ws)
  | len >= wLen	= dropLine (len - wLen - 1) ws
  | otherwise	= w : ws
  where wLen = length w

splitLines :: Int -> [Word] -> [Line]
splitLines _ []		= []
splitLines lineLen st	= (getLine lineLen st) : splitLines lineLen (dropLine lineLen st)

fill :: Int -> String -> [Line]
fill lineLen = (splitLines lineLen) . splitWords

addSpaces :: Line -> String
addSpaces []		= []
addSpaces [w]		= w
addSpaces (w:wl)	= w ++ " " ++ addSpaces wl

joinLines :: Int -> [Line] -> String
joinLines _ []			= []
joinLines _ [l]			= addSpaces l ++ "\n"
joinLines lineLen (l:wl)	= joinLine lineLen l ++ "\n" ++ (joinLines lineLen wl)
-- </definitions-from-book>

-- Exercise 7.23, page 133

-- first arg: nr. of places in which to place spaces
-- second arg: nr. of spaces to be placed
getSpaceDistribution :: Int -> Int -> [Int]
getSpaceDistribution m n
  | m == 0	= []
  | n < m	= []
  | otherwise	= zipWith (+) ((replicate left 1) ++ (replicate (m - left) 0)) (replicate m nrSp)
    where
    nrSp = n `div` m
    left = n - (nrSp * m)

-- returns a list of String's containing whitespace of various length
-- based on the space distribution provided as parameter
getSpaces :: [Int] -> [String]
getSpaces []		= []
getSpaces (x:xs)	= (replicate x ' ') : (getSpaces xs)

jl :: [Int] -> [String] -> String
jl spaceDist words = concat ((head words) : (zipWith (++) (getSpaces spaceDist) (tail words)))

joinLine :: Int -> [String] -> String
joinLine lineLen words = jl (getSpaceDistribution ((length words) - 1) (lineLen - length (concat words))) words

-- <my-functions>
justify :: Int -> String -> String
justify lineLen text = joinLines lineLen (fill lineLen text)
-- </my-functions>

-- Exercise 7.24, page 133

wc :: String -> (Int,Int,Int)
wc []			= (0,0,0)
wc [ch]
  | elem ch "\t\n "	= (1,0,1)
  | otherwise		= (1,1,1)
wc (cc:nc:st)
  | elem cc "\t "	= (chars + 1,words,lines)
  | cc == '\n'		= (chars + 1,words,lines+1)
  | elem nc "\t\n "	= (chars + 1,words+1,lines)
  | otherwise		= (chars + 1,words,lines)
  where
  (chars,words,lines)	= wc (nc:st)

wcFormat :: Int -> String -> (Int,Int,Int)
wcFormat lineLen st = wcLines (fill lineLen st)

wcLines :: [Line] -> (Int,Int,Int)
wcLines text = (wcChars (concat text),length (concat text),length text)

wcChars :: [Word] -> Int
wcChars []	= 0
wcChars (w:ws)	= length w + 1 + wcChars ws

-- Exercise 7.25, page 133

palindrome :: String -> Bool
palindrome w = w == reverse w

isPalindrome :: String -> Bool
isPalindrome w = palindrome [toLower x | x <- w, isAlpha x]

-- Exercise 7.26, page 133

subst :: String -> String -> String -> String
subst _ _ []			= []
subst oldSub newSub (x:xs)
  | oldSub == take len (x:xs)	= newSub ++ drop len (x:xs)
  | otherwise			= x : subst oldSub newSub xs
  where
  len = length oldSub