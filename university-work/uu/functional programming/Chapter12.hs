{-
	Bogdan Dumitriu
-}

import Prelude hiding (compare)

-- Exercise 12.2, page 214

numEqual :: Eq a => a -> [a] -> Int
numEqual x = length . filter (\y -> x == y)

member :: Eq a => [a] -> a -> Bool
member list x = numEqual x list > 0

-- Exercise 12.4, page 219

class Visible a where
  toString :: a -> String
  size :: a -> Int

instance Visible Bool where
  toString True = "True"
  toString False = "False"
  size _ = 1

instance (Visible a, Visible b) => Visible (a, b) where
  toString (a, b) = "(" ++ toString a ++ ", " ++ toString b ++ ")"
  size (a, b) = size a + size b

instance (Visible a, Visible b, Visible c) => Visible (a, b, c) where
  toString (a, b, c) = "(" ++ toString a ++ ", " ++ toString b ++ ", " ++ toString c ++ ")"
  size (a, b, c) = size a + size b + size c

printVis :: Visible a => a -> IO()
printVis x = (print . toString) x

-- Exercise 12.5, page 219

instance Visible Int where
  toString x = show x
  size _ = 1

-- Exercise 12.6, page 219

--compare :: (Visible a, Visible b) => a -> b -> Bool
compare x y = size x <= size y

-- Exercise 12.8, page 219

{-
instance (Ord a, Ord b) => Ord (a, b) where
  compare (a1, b1) (a2, b2) = let cr = compare a1 a2 in if cr == EQ then compare b1 b2 else cr

instance Ord b => Ord [b] where
  compare [] []		= EQ
  compare _ []		= GT
  compare [] _		= LT
  compare (x:xs) (y:ys)	= let cr = compare x y in if cr == EQ then compare xs ys else cr
-}

-- Exercise 12.10, page 225

printBoolFun :: (Bool -> Bool) -> IO()
printBoolFun = putStr . showBoolFun

showBoolFun :: (Bool -> Bool) -> String
showBoolFun f =
	"+---+-----+\n" ++
	"| x | f x |\n" ++
	"+---+-----+\n" ++
	"| F |  " ++ (if f False then "T" else "F") ++ "  |\n" ++
	"| T |  " ++ (if f True then "T" else "F") ++ "  |\n" ++
	"+---+-----+\n"

showBoolFunGen :: (a -> String) -> (Bool -> a) -> String
showBoolFunGen g f =
	"+---+-----\n" ++
	"| x | f x \n" ++
	"+---+-----\n" ++
	"| F | " ++ g (f False) ++ "\n" ++
	"| T | " ++ g (f True) ++ "\n" ++
	"+---+-----\n"

showMABoolFun :: Int -> ([Bool] -> Bool) -> String
showMABoolFun nrArgs f = seps ++ header ++ seps ++ (concat . map (displayLine f) $ (getBoolTuples nrArgs)) ++ seps
  where
  seps		= "+-" ++ dashes ++ "+---+\n"
  header	= "|" ++ xs  ++ " | f |\n"
  xs		= concat . map (\x -> " x" ++ show x) $ [1 .. nrArgs]
  dashes	= replicate (length xs) '-'

displayLine :: ([Bool] -> Bool) -> [Bool] -> String
displayLine f args = "|" ++ (concat . map (\x -> "  " ++ if x then "T" else "F") $ args) ++ " | " ++ (if f args then "T" else "F") ++ " |\n"

getBoolTuples :: Int -> [[Bool]]
getBoolTuples 0		= []
getBoolTuples 1		= [[False], [True]]
getBoolTuples n		= map (\x -> False : x) r ++ map (\x -> True : x) r
  where
  r = getBoolTuples (n-1)

-- Exercise 12.11, page 225

{-
instance Show (Bool -> Bool) where
  show f = showBoolFun f
-}
