{-
	Bogdan Dumitriu
	October 21st, 2004
-}

import Char

type Symbol	= Char
type NonTerm	= Symbol
type Terminal	= Symbol

nonterm :: Symbol -> Bool
nonterm = isUpper

terminal :: Symbol -> Bool
terminal = not . isUpper

type Prod = (NonTerm, [[Symbol]])
type Gram = [Prod]

exprs :: Gram
exprs = [
	('E', ["T+E", "T"]),
	('T', ["F*T", "F"]),
	('F', ["B", "(E)"]),
	('B', ["0", "1"])
	]

lists :: Gram
lists = [
	('L', ["B", "L,L"]),
	('B', ["0", "1"])
	]

(?) :: Eq a => a -> [(a, b)] -> b
x ? list = maybe (error "element not found in list.") id (lookup x list)

--
-- If we were supposed to have our own implementation for the ? operator (as opposed to using
-- lookup), here it is...
--
{-
(?) :: Eq a => a -> [(a, b)] -> b
x ? []		= error "element not found in list."
x ? ((a, b):xs)
  | x == a	= b
  | otherwise	= x ? xs
-}

data Tree a b = Node a [Tree a b] | Leaf b
	      deriving (Eq, Ord, Show)

type Ptree = Tree NonTerm Symbol

leaves :: Tree a b -> [b]
leaves (Leaf x)		= [x]
leaves (Node _ list)	= concat (map leaves list)

nodes :: Tree a b -> [a]
nodes (Leaf _)		= []
nodes (Node x list)	= x : concat (map nodes list)

--
-- I have modified depth a bit in order for it to also handle nodes with an empty list of subtrees,
-- even if we're not particularly interested in that case. It's there just for the sake of completeness.
--
depth :: Tree a b -> Int
depth (Leaf _)		= 1
depth (Node _ [])	= 1
depth (Node _ list)	= 1 + maximum (map depth list)

combs :: [a] -> [b] -> [(a, b)]
combs xs ys = [(x, y) | x <- xs, y <- ys]

combslist :: [[a]] -> [[a]]
combslist = foldr f [[]]
  where
  f xs e = [x:y | x <- xs, y <- e]

gen :: Gram -> Int -> Symbol -> [Ptree]
gen gram 1 symbol	= [Leaf symbol]
gen gram n symbol
  | terminal symbol	= [Leaf symbol]
  | otherwise		= Leaf symbol : concat (map (map (Node symbol)) allTrees)
  where
  allTrees = map (genlist gram (n-1)) (symbol ? gram)

genlist :: Gram -> Int -> [Symbol] -> [[Ptree]]
genlist gram n symbols = combslist (map (gen gram n) symbols)

parse :: Gram -> Int -> [Terminal] -> [Ptree]
parse gram n st = filter ((==) st . leaves) (gen gram n (fst . head $ gram))