module Chapter3 where

import Data.List

myLength        :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength []     = 0

mySum        :: (Num a) => [a] -> a
mySum (x:xs) = x + mySum xs
mySum []     = 0

meanList   :: (Fractional a) => [a] -> a
meanList l = mySum l / fromIntegral (myLength l)

palindromize        :: [a] -> [a]
palindromize (x:xs) = (x:palindromize xs ++ [x])
palindromize []     = []

palindrome         :: (Eq a) => [a] -> Bool
palindrome []      = True
palindrome [x]     = True
palindrome (x:xs)
  | x == last xs = palindrome (init xs)
  | otherwise    = False

sortBySLLength :: [[a]] -> [[a]]
sortBySLLength l = sortBy (\x y -> compare (length x) (length y)) l

myIntersperse            :: a -> [[a]] -> [a]
myIntersperse sep []     = []
myIntersperse sep [x]    = x
myIntersperse sep (x:xs) = x ++ (sep : myIntersperse sep xs)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight                     :: Tree a -> Int
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)
treeHeight Empty               = 0

data Point = Point Int Int
             deriving (Show, Eq)
data Direction = DLeft | DRight | DStraight
                 deriving (Show, Eq)

turn :: Point -> Point -> Point -> Direction
turn (Point x1 y1) (Point x2 y2) (Point x3 y3)
  | diff == 0 = DStraight
  | diff < 0  = DLeft
  | diff > 0  = DRight
  where diff = (x3 - x1) * (y2 - y1) - (x2 - x1) * (y3 - y1)

turns            :: [Point] -> [Direction]
turns (x:y:z:xs) = (turn x y z : turns (y:z:xs))
turns _          = []

grahamScan        :: [Point] -> [Point]
grahamScan points = sortedScan (sortBy compAngle (nub points) ++ [p])
  where p@(Point px py) = findFirst points

        findFirst [] = error "list cannot be empty"
        findFirst l  = foldl1 minPoint l

        minPoint p1@(Point x1 y1) p2@(Point x2 y2)
          | y1 < y2 || (y1 == y2 && x1 <= x2) = p1
          | otherwise                         = p2

        -- 1. cotangent of angle formed by p & p1 with the axis is (x1-py)/(y1-py)
        -- 2. the cotangent is a monotonically decreasing function, so ctan(a) < ctan(b) => a > b
        -- 3. p has to be at the front of the list, hence the special case
        compAngle p1@(Point x1 y1) p2@(Point x2 y2) =
          if p1 == p then LT else if p2 == p then GT else compare right left
          where left = (x1 - px) * (y2 - py)
                right = (x2 - px) * (y1 - py)

sortedScan               :: [Point] -> [Point]
sortedScan (x:xs)
  | length xs <= 1       = [x]
sortedScan (x:y:z:xs)
  | turn x y z == DRight = sortedScan (x:z:xs)
  | otherwise            = (x : sortedScan (y:z:xs))

testGraham = grahamScan [Point 2 1, Point 5 3, Point 3 5, Point 3 2, Point 6 3, Point 4 1, Point 4 5, Point 4 3, Point 4 0, Point 8 0]

--main = putStrLn "Hello, World!"
