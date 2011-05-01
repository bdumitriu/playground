{-
	Bogdan Dumitriu
-}

data Edit = Change Char | Copy | Delete | Insert Char | Kill
	deriving (Eq, Show)

transform :: String -> String -> [Edit]
transform [] []		= []
transform xs []		= [Kill]
transform [] ys		= map Insert ys
transform (x:xs) (y:ys)
  | x == y	= Copy : transform xs ys
  | otherwise	= best [Delete : transform xs (y:ys),
			Insert y : transform (x:xs) ys,
			Change y : transform xs ys]

best :: [[Edit]] -> [Edit]
best [x] = x
best (x:xs)
  | cost x <= cost b	= x
  | otherwise		= b
  where
  b = best xs

cost :: [Edit] -> Int
cost = length . filter (/= Copy)