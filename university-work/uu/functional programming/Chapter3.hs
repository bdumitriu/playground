{-
	Bogdan Dumitriu
-}

import Hugs.Prelude
import Char

-- Exercise 3.1, page 35

exOr1 :: Bool -> Bool -> Bool
exOr1 x y = ((x && not y) || (not x && y))

-- Exercise 3.1, page 35

exOr2 :: Bool -> Bool -> Bool
exOr2 True True = False
exOr2 True False = True
exOr2 False True = True
exOr2 False False = False

-- Exercise 3.4, page 35

nAnd1 :: Bool -> Bool -> Bool
nAnd1 x y = not (x && y)

nAnd2 :: Bool -> Bool -> Bool
nAnd2 True True = False
nAnd2 True False = True
nAnd2 False True = True
nAnd2 False False = True

-- Exercise 3.6, page 37

threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m == n) && (n == p)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p = (m /= n) && (n /= p) && (m /= p)

-- Exercise 3.8, page 37

fourEqual1 :: Int -> Int -> Int -> Int -> Bool
fourEqual1 m n p q = (threeEqual m n p) && (m == q)

fourEqual2 :: Int -> Int -> Int -> Int -> Bool
fourEqual2 m n p q = (m == n) && (n == p) && (p == q)

-- Exercise 3.11, page 41

min :: Int -> Int -> Int
min x y
  | x <= y	= x
  | otherwise	= y

minThree :: Int -> Int -> Int -> Int
minThree x y z
  | (x <= y) && (x <= z)	= x
  | (y <= z)			= y
  | otherwise			= z

-- Exercise 3.12, page 43

offset :: Int
offset = ord 'a' - ord 'A'

toUpper :: Char -> Char
toUpper ch
  | ('a' <= ch) && (ch <= 'z')	= chr (ord ch - offset)
  | otherwise			= ch

-- Exercise 3.13, page 43

numOffset :: Int
numOffset = ord '0'

charToNum :: Char -> Int
charToNum ch
  | ('0' <= ch) && (ch <= '9')	= ord ch - numOffset
  | otherwise			= 0

-- Exercise 3.14, page 45

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromInt (x + y + z) / 3

aboveAverage :: Int -> Float -> Int
aboveAverage x avg
  | fromInt x >= avg	= 1
  | otherwise		= 0

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z =
	aboveAverage x (averageThree x y z) + aboveAverage y (averageThree x y z) + aboveAverage z (averageThree x y z)

-- Exercise 3.15, page 46

numberNDRoots :: Float -> Float -> Float -> Int
numberNDRoots a b c
  | b^2 > 4.0 * a * c	= 2
  | b^2 == 4.0 * a * c	= 1
  | b^2 < 4.0 * a * c	= 0


numberDRoots :: Float -> Float -> Int
numberDRoots b c
  | b /= 0.0			= 1
  | b == 0.0 && c /= 0.0	= 0
  | otherwise			= 3

-- Exercise 3.16, page 46

numberRoots :: Float -> Float -> Float -> Int
numberRoots a b c
  | a == 0.0	= numberDRoots b c
  | otherwise	= numberNDRoots a b c

-- Exercise 3.17, page 46

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
  | numberRoots a b c == 0			= 0
  | (a /= 0.0) && numberRoots a b c == 1	= (-b) / (2 * a)
  | (a == 0.0) && numberRoots a b c == 1	= (-c) / b
  | numberRoots a b c == 2			= (-b) - sqrt (b^2 - 4 * a * c) / (2 * a)

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
  | numberRoots a b c == 0			= 0
  | (a /= 0.0) && numberRoots a b c == 1	= (-b) / (2 * a)
  | (a == 0.0) && numberRoots a b c == 1	= (-c) / b
  | numberRoots a b c == 2			= (-b) + sqrt (b^2 - 4 * a * c) / (2 * a)
