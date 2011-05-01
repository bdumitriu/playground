module Marks (Direction (..), 
	      intMarkToDir, dirToIntMark,
	      boolMarkToDir, dirToBoolMark,
	      boolMarkToIntMark, intMarkToBoolMark,
	     ) where

import Data.Bits

data Direction = NoDir | Dir Int | SpecialDir
	       deriving (Show,Eq)

intMarkToDir :: Int -> Direction
intMarkToDir 0 = NoDir
intMarkToDir 7 = SpecialDir
intMarkToDir 5 = Dir 0
intMarkToDir 6 = Dir 1
intMarkToDir 3 = Dir 2
intMarkToDir 4 = Dir 5
intMarkToDir 2 = Dir 3
intMarkToDir 1 = Dir 4
intMarkToDir n = error ("tried to convert an invalid number "++show n
			++" to a direction")

dirToIntMark :: Direction -> Int
dirToIntMark (Dir 0) = 5
dirToIntMark (Dir 1) = 6
dirToIntMark (Dir 2) = 3
dirToIntMark (Dir 3) = 2
dirToIntMark (Dir 4) = 1
dirToIntMark (Dir 5) = 4
dirToIntMark (Dir n) = error ("invalid direction "++show (Dir n))
dirToIntMark NoDir = 0
dirToIntMark SpecialDir = 7

boolMarkToIntMark :: (Bool,Bool,Bool) -> Int
boolMarkToIntMark (b0,b1,b2) = boolToInt b0 + 2*boolToInt b1 + 4*boolToInt b2

intMarkToBoolMark :: Int -> (Bool,Bool,Bool)
intMarkToBoolMark n = (testBit n 0, testBit n 1, testBit n 2)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

boolMarkToDir :: (Bool,Bool,Bool) -> Direction
boolMarkToDir = intMarkToDir . boolMarkToIntMark

dirToBoolMark :: Direction -> (Bool,Bool,Bool)
dirToBoolMark = intMarkToBoolMark . dirToIntMark

