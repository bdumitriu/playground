{-
	Bogdan Dumitriu
-}

import Pictures hiding (superimpose)

type Position = (Int,Int)
type Image = (Picture,Position)

-- general use functions

transpose :: [[a]] -> [[a]]
transpose []		= []
transpose [x]		= map return x
transpose (x:xs) 	= zipWith (:) x (transpose xs)

-- Exercise 6.1, page 98

superimposeChar :: Char -> Char -> Char
superimposeChar x y
  | (x == '.') && (y == '.')	= '.'
  | otherwise			= '#'

-- Exercise 6.2, page 98

superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine x y = [superimposeChar i j | (i,j) <- zip x y]

-- Exercise 6.3, page 99

superimpose :: Picture -> Picture -> Picture
superimpose x y = [superimposeLine i j | (i,j) <- zip x y]

-- Exercise 6.6, page 99

rotate90 :: Picture -> Picture
rotate90 = transpose . flipH

-- Exercise 6.7, page 99

rotate270 :: Picture -> Picture
rotate270 = transpose . flipV

-- Exercise 6.8, page 100

scaleList :: [a] -> Int -> [a]
scaleList list n = concat [replicate n x | x <- list]

scale :: Picture -> Int -> Picture
scale pic n
  | n <= 0	= []
  | otherwise	= scaleList [scaleList x n | x <- pic] n

-- Exercise 6.9, page 101

makeImage :: Picture -> Position -> Image
makeImage x y = (x,y)

-- Exercise 6.10, page 101

changePosition :: Image -> Position -> Image
changePosition (x,y) z = (x,z)

-- Exercise 6.11, page 101

movePosition :: Image -> Int -> Int -> Image
movePosition (z,(x,y)) dx dy = (z,(x+dx,y+dy))

-- Exercise 6.12, page 101

printImage :: Image -> IO()
printImage (x,y) = printPicture x

-- Exercise 6.13, page 102

naiveFlipH :: Image -> Image
naiveFlipH (x,y) = (flipH x,y)

naiveFlipV :: Image -> Image
naiveFlipV (x,y) = (flipV x,y)

naiveRotate :: Image -> Image
naiveRotate (x,y) = (rotate x,y)

naiveRotate90 :: Image -> Image
naiveRotate90 (x,y) = (rotate90 x,y)

-- Exercise 6.14, page 102

geomFlipH :: Image -> Image
geomFlipH (x,(xx,yy)) = (flipH x,(xx,yy - length x))

geomFlipV :: Image -> Image
geomFlipV (x,(xx,yy)) = (flipV x,(xx - length (head x),yy))

geomRotate :: Image -> Image
geomRotate (x,(xx,yy)) = (rotate x,(xx - length (head x),yy - length x))

geomRotate90 :: Image -> Image
geomRotate90 (x,(xx,yy)) = (rotate90 x,(xx,yy - length (head x)))

-- Exercise 6.15, page 102

padTop :: Image -> Int -> Image
padTop (pic,pos) padding = ((replicate padding (replicate (length (head pic)) '.')) ++ pic,pos)

padBottom :: Image -> Int -> Image
padBottom (pic,(x,y)) padding = (pic ++ (replicate padding (replicate (length (head pic)) '.')),(x,y - padding))

padLeft :: Image -> Int -> Image
padLeft (pic,(x,y)) padding = (map (pad ++) pic,(x - padding,y))
  where
  pad = replicate padding '.'

padRight :: Image -> Int -> Image
padRight (pic,pos) padding = (map (++ pad) pic,pos)
  where
  pad = replicate padding '.'
  
-- pad img top bottom left right => new img
pad :: Image -> Int -> Int -> Int -> Int -> Image
pad img top bottom left right = padRight (padLeft (padBottom (padTop img top) bottom) left) right

-- Exercise 6.16, page 103

superimposeImages :: Image -> Image -> Image
superimposeImages (pic1,(x1,y1)) (pic2,(x2,y2)) =
	(superimpose
		(fst (pad (pic1,(x1,y1)) top1 bottom1 left1 right1))
		(fst (pad (pic2,(x2,y2)) top2 bottom2 left2 right2))
	,
	(minX,minY))
  where
  xx1 = x1 + (length (head pic1))
  yy1 = y1 + (length pic1)
  xx2 = x2 + (length (head pic2))
  yy2 = y2 + (length pic2)
  minX = min x1 x2
  minY = min y1 y2
  maxX = max xx1 xx2
  maxY = max yy1 yy2
  top1 = maxY - yy1
  top2 = maxY - yy2
  bottom1 = y1 - minY
  bottom2 = y2 - minY
  left1 = x1 - minX
  left2 = x2 - minX
  right1 = maxX - xx1
  right2 = maxX - xx2
