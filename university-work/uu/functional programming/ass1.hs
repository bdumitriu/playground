-- first variant

{-
	Takes a list of Int's (x) and a list of lists of Int's (y) as parameters.
	Returns a list of lists of Int's based on y, modified by adding the i-th
	element of list x on the first position of the i-th list of list y. Lists
	x and y are expected to have the same length, except for the case when y
	is the empty list.

	For example,
		placefirst [1,2,3] [[4,5],[6,7],[8,9]]
	yields
		[[1,4,5],[2,6,7],[3,8,9]]

	and
		placefirst [1,2,3] []
	yields
		[[1],[2],[3]]
-}
placefirst :: [Int] -> [[Int]] -> [[Int]]
placefirst [] []		= []
placefirst (x:restX) []		= [x] : (placefirst restX [])
placefirst (x:restX) (y:restY)	= (x:y) : (placefirst restX restY)

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
transpose (x:s)	= placefirst x (transpose s)

-- second variant

transpose2 :: [[a]] -> [[a]]
transpose2 [] = []
transpose2 ([] : xxs) = transpose2 xxs
transpose2 ((x:xs) : xxs) = (x : [h | (h:t) <- xxs]) : transpose2 (xs : [t | (h:t) <- xxs])

-- third variant

transpose3 :: [[Int]] -> [[Int]]
transpose3 []		= []
transpose3 [x]		= map return x
transpose3 (x:xs)	= zipWith (:) x (transpose3 xs)

-- fourth variant

transpose4 :: [[Int]] -> [[Int]]
transpose4 []		= []
transpose4 [x]		= map (\x -> [x]) x
transpose4 (x:s)	= zipWith (:) x (transpose4 s)