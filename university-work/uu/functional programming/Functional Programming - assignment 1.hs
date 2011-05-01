{-
	Assignment 1
	Bogdan Dumitriu
	22 September 2004
-}

{-
	Takes a list of lists of Int's representing a matrix and returns its
	transpose.
	
	For example,
		transpose [[1,2],[3,4],[5,6],[7,8]]
	returns
		[[1,3,5,7],[2,4,6,8]]
-}
transpose :: [[Int]] -> [[Int]]
transpose []	= []
transpose [x]	= map (\x -> [x]) x
transpose (x:s)	= zipWith (:) x (transpose s)
