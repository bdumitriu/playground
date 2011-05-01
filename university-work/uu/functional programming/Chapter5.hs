{-
	Bogdan Dumitriu
-}

import Hugs.Prelude
import Char hiding (toUpper)

-- general use functions

offset :: Int
offset = ord 'a' - ord 'A'

toUpperSingle :: Char -> Char
toUpperSingle ch
  | ('a' <= ch) && (ch <= 'z')	= chr (ord ch - offset)
  | otherwise			= ch

-- Exercise 5.1, page 76

maxOccurs :: Int -> Int -> (Int,Int)
maxOccurs x y
  | x == y	= (x,2)
  | x < y	= (y,1)
  | otherwise	= (x,1)

maxThreeOccurs :: Int -> Int -> Int -> (Int,Int)
maxThreeOccurs x y z
  | (x == y) && (y == z)	= (x,3)
  | (x == y) && (y < z)		= (z,1)
  | (x == y) && (y > z)		= (x,2)
  | otherwise			= maxOccurs (fst (maxOccurs x y)) z

-- Exercise 5.2, page 76

maxThree :: (Int,Int,Int) -> Int
maxThree (x,y,z)
  | (x >= y) && (x >= z)	= x
  | (y >= x) && (y >= z)	= y
  | (z >= x) && (z >= y)	= z

minThree :: (Int,Int,Int) -> Int
minThree (x,y,z)
  | (x <= y) && (x <= z)	= x
  | (y <= x) && (y <= z)	= y
  | (z <= x) && (z <= y)	= z

middleThree :: (Int,Int,Int) -> Int
middleThree (x,y,z)
  | (x >= y) && (x <= z)	= x
  | (x >= z) && (x <= y)	= x
  | (y >= x) && (y <= z)	= y
  | (y >= z) && (y <= x)	= y
  | (z >= x) && (z <= y)	= z
  | (z >= y) && (z <= x)	= z

orderTriple :: (Int,Int,Int) -> (Int,Int,Int)
orderTriple (x,y,z) = (minThree (x,y,z), middleThree (x,y,z), maxThree (x,y,z))

-- Exercise 5.3, page 76

-- We use the equation y = m*x + n for the definition of the line, with m and n
-- being the two supplied parameters.

intersectionX :: (Int,Int) -> Float
intersectionX (m,n)
  | m == 0	= error "line doesn't intersect the x axis"
  | otherwise	= - (fromInt n) / (fromInt m)

-- Exercise 5.8, page 82

doubleAll :: [Int] -> [Int]
doubleAll s = [2*x | x <- s]

-- Exercise 5.9, page 82

capitalize :: String -> String
capitalize s = [toUpperSingle x | x <- s]

capitalizeLetters :: String -> String
capitalizeLetters s = [toUpperSingle x | x <- s, isAlpha x]

-- Exercise 5.10, page 82

divisors :: Int -> [Int]
divisors n
  | n <= 0	= []
  | otherwise	= [x | x <- [1 .. n], (n `mod` x) == 0]


isPrime :: Int -> Bool
isPrime n
  | n <= 0	= False
  | n == 1	= True
  | otherwise	= length (divisors n) == 2


primes :: Int -> [Int]
primes n
  | n <= 0	= []
  | otherwise	= [x | x <- [1 .. n], isPrime x]

-- Exercise 5.10, page 82

matches :: Int -> [Int] -> [Int]
matches n s = [x | x <- s, x == n]

elem :: Int -> [Int] -> Bool
elem n s = length (matches n s) > 0

-- Exercise 5.19, page 94

toUpper :: String -> String
toUpper [] = []
toUpper (x:xs) = (toUpperSingle x) : (toUpper xs)

-- Exercise 5.20, page 94

romanDigit :: Char -> String
romanDigit ch
  | ch == '1'	= "I"
  | ch == '2'	= "II"
  | ch == '3'	= "III"
  | ch == '4'	= "IV"
  | ch == '5'	= "V"
  | ch == '6'	= "VI"
  | ch == '7'	= "VII"
  | ch == '8'	= "VIII"
  | ch == '9'	= "IX"
  | otherwise	= ""

-- Exercise 5.21, page 94

onThreeLines :: String -> String -> String -> String
onThreeLines x y z	= x ++ "\n" ++ y ++ "\n" ++ z

-- Exercise 5.22, page 94

onSeparateLines :: [String] -> String
onSeparateLines []	= ""
onSeparateLines (x:xs)	= x ++ "\n" ++ onSeparateLines xs

-- Exercise 5.23, page 94

duplicate :: String -> Int -> String
duplicate s x
  | x <= 0	= ""
  | otherwise	= s ++ duplicate s (x - 1)

-- Exercise 5.24, page 94

pushRight :: Int -> String -> String
pushRight x s
  | x - length s <= 0	= s
  | otherwise		= " " ++ pushRight (x - 1) s

-- Exercise 5.26, page 94

fibonacci :: Int -> Int
fibonacci n
  | n < 0	= 0
fibonacci 0	= 0
fibonacci 1	= 1
fibonacci n	= fibonacci (n - 1) + fibonacci (n - 2)

fibTable :: Int -> String
fibTable n
  | n < 0	= pushRight 10 "n" ++ pushRight 10 "fib n"
fibTable n	= fibTable (n - 1) ++ "\n" ++ pushRight 10 (show n) ++ pushRight 10 (show (fibonacci n))

