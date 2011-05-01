{-
	Bogdan Dumitriu, Sep 2004
-}

-- Exercise 2.1, page 29

module UsePictures where
import Pictures

blackHorse :: Picture
blackHorse = invertColour horse

rotateHorse :: Picture
rotateHorse = rotate horse

-- Exercise 2.2, page 29

blackrec :: Int -> Int -> Picture
blackrec 1 1 = ["#"]
blackrec 1 n =
	if (n > 0)
	then ['#' : head (blackrec 1 (n - 1))]
	else blackrec 1 1
blackrec m n =
	if (n > 0 && m > 1)
	then above (blackrec 1 n) (blackrec (m - 1) n)
	else blackrec 1 1

black :: Picture
black = blackrec 12 12

-- Exercise 2.3, page 29

fourboxes1 :: Picture
fourboxes1 = above (sideBySide white black) (sideBySide black white)

fourboxes2 :: Picture
fourboxes2 = sideBySide (above white black) (above black white)

sixteenboxes :: Picture
sixteenboxes = above (sideBySide fourboxes1 fourboxes1) (sideBySide fourboxes1 fourboxes1)

chessBoard :: Picture
chessBoard = above (sideBySide sixteenboxes sixteenboxes) (sideBySide sixteenboxes sixteenboxes)

-- Exercise 2.4, page 30

pic1 :: Picture
pic1 = above (sideBySide horse (invertColour horse)) (sideBySide (invertColour horse) horse)

pic2 :: Picture
pic2 = above (sideBySide horse (invertColour horse)) (sideBySide (invertColour (flipV horse)) (flipV horse))

pic3 :: Picture
pic3 = above (sideBySide horse (invertColour horse)) (sideBySide (invertColour (rotate horse)) (rotate horse))

-- Exercise 2.5, page 31

pic4 :: Picture
pic4 = above (sideBySide horse (invertColour horse)) (sideBySide (invertColour (flipV (rotate horse))) (flipV (rotate horse)))