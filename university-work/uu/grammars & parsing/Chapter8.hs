import Parser
import Char

data Tree = Leaf Int
	  | Bin Tree Tree
	  deriving (Eq, Ord, Show)

exTree :: Tree
exTree = Bin (Bin (Bin (Leaf 3) (Leaf 4)) (Leaf 5)) (Leaf 8)

type TreeAlgebra a = (Int -> a, a -> a -> a)

foldTree :: TreeAlgebra a -> Tree -> a
foldTree (leaf, bin) = fold
  where
  fold (Leaf x)		= leaf x
  fold (Bin u v)	= bin (fold u) (fold v)

--
-- Exercise 8.1
--
heightAlgebra :: TreeAlgebra Int
heightAlgebra = (const 1, \x y -> x `max` y + 1)

height :: Tree -> Int
height = foldTree heightAlgebra

frontAtLevelAlgebra :: TreeAlgebra (Int -> [Int])
frontAtLevelAlgebra = (\x lvl -> if lvl == 1 then [x] else [],
		       \u v lvl -> if (lvl > 1) then u (lvl-1) ++ v (lvl-1) else [])

frontAtLevel :: Tree -> Int -> [Int]
frontAtLevel t = foldTree frontAtLevelAlgebra t

deepestFront :: Tree -> [Int]
deepestFront t = frontAtLevel t (height t)

infix 9 `tuple`

tuple :: TreeAlgebra a -> TreeAlgebra b -> TreeAlgebra (a, b)
(la, ba) `tuple` (lb, bb) = (\x -> (la x, lb x), \u v -> (ba (fst u) (fst v), bb (snd u) (snd v)))

deepestFront' :: Tree -> [Int]
deepestFront' t = df h
  where
  (df, h) = foldTree (tuple frontAtLevelAlgebra heightAlgebra) t

hfAlgebra :: TreeAlgebra (Int -> (Int, [Int]))
hfAlgebra = (\x lvl -> (1, if lvl == 1 then [x] else []),
	     \u v lvl -> let (hu, dfu) = u (lvl-1)
			     (hv, dfv) = v (lvl-1)
			 in (hu `max` hv + 1, if lvl > 1 then dfu ++ dfv else []))

deepestFront'' :: Tree -> [Int]
deepestFront'' t = df
  where
  (h, df) = foldTree hfAlgebra t h

--
-- Exercise 8.2
--
lowestAlgebra :: TreeAlgebra Int
lowestAlgebra = (const 1, \x y -> x `min` y + 1)

lowest :: Tree -> Int
lowest = foldTree lowestAlgebra

highestFront :: Tree -> [Int]
highestFront t = frontAtLevel t (lowest t)

highestFront' :: Tree -> [Int]
highestFront' t = hf l
  where
  (l, hf) = foldTree (lowestAlgebra `tuple` frontAtLevelAlgebra) t

lfAlgebra :: TreeAlgebra (Int -> (Int, [Int]))
lfAlgebra = (\x lvl -> (1, if lvl == 1 then [x] else []),
	     \u v lvl -> let (lu, lfu) = u (lvl-1)
			     (lv, lfv) = v (lvl-1)
			 in (lu `min` lv + 1, if lvl > 1 then lfu ++ lfv else []))

highestFront'' :: Tree -> [Int]
highestFront'' t = hf
  where
  (l, hf) = foldTree lfAlgebra t l

--
-- A small compiler
--

--
-- Exercises 8.3, 8.4
--

data ExprAS = If ExprAS ExprAS ExprAS
	    | Apply ExprAS ExprAS
	    | ConInt Int
	    | ConBool Bool
	    | Var String
	    | Def String ExprAS ExprAS
	    deriving (Eq, Ord, Show)

type ExprASAlgebra a = (a -> a -> a -> a, a -> a -> a, Int -> a, Bool -> a, String -> a, String -> a -> a -> a)

foldExprAS :: ExprASAlgebra a -> ExprAS -> a
foldExprAS (iff, apply, conint, conbool, var, def) = fold
  where
  fold (If ce te ee)	= iff (fold ce) (fold te) (fold ee)
  fold (Apply fe ae)	= apply (fold fe) (fold ae)
  fold (ConInt i)	= conint i
  fold (ConBool b)	= conbool b
  fold (Var x)		= var x
  fold (Def x u v)	= def x (fold u) (fold v)

sptoken :: String -> Parser Char String
sptoken s = (\_ b _ -> b) <$> many (symbol ' ') <*> token s <*> many1 (symbol ' ')

boolean :: Parser Char Bool
boolean = const True <$> token "True"
	<|> const False <$> token "False"

parseExpr :: Parser Char ExprAS
parseExpr = expr0
  where
  expr0 = (\_ x _ y _ z -> If x y z) <$> sptoken "if" <*> parseExpr <*> sptoken "then" <*> parseExpr <*> sptoken "else" <*> parseExpr <|> expr1
  expr1 = chainl expr2 (const Apply <$> many1 (symbol ' ')) <|> expr2
  expr2 = ConBool <$> boolean <|> ConInt <$> natural <|> Var <$> identifier <|> (\_ x y z -> Def x y z) <$> sptoken "def" <*> identifier <*> expr0 <*> expr0

natural :: Parser Char Int
natural = read <$> greedy1 dig

dig :: Parser Char Char
dig = satisfy isDigit

uLetter :: Parser Char Char
uLetter = satisfy isLower

cLetter :: Parser Char Char
cLetter = satisfy isUpper

letter :: Parser Char Char
letter = uLetter <|> cLetter

identifier :: Parser Char [Char]
identifier = list <$> letter <*> sos

sos :: Parser Char [Char]
sos = many (dig <|> letter)

data InstructionSM = LoadInt Int
		   | LoadBool Bool
		   | LoadVar String
		   | Call
		   | SetLabel Label
		   | BrFalse Label
		   | BrAlways
		   deriving (Eq, Ord, Show)

type Label = Int

compile :: ExprAS -> Label -> ([InstructionSM], Label)
compile = foldExprAS compileAlgebra

compileAlgebra :: ExprASAlgebra (Label -> ([InstructionSM], Label))
compileAlgebra = (\cce cte cee -> \l -> let (ce, l') = cce (l+2)
					    (te, l'') = cte l'
					    (ee, l''') = cee l''
					in
					(
					ce ++
					[BrFalse l] ++
					te ++
					[BrAlways] ++
					[SetLabel l] ++
					ee ++
					[SetLabel (l+1)]
					, l+2),
		  \cf cx -> \l -> let (x, l') = cx l
		                      (f, l'') = cf l'
				  in (x ++ f ++ [Call], l''),
		  \i -> \l -> ([LoadInt i], l),
		  \b -> \l -> ([LoadBool b], l),
		  \x -> \l -> ([LoadVar x], l),
		  \_ _ _ -> \l -> ([], l)
		 )

compile_ :: String -> [InstructionSM]
compile_ s = i
  where
  e = if null . parseExpr $ s then error "no parse" else fst . head . parseExpr $ s
  (i, _) = compile e 0