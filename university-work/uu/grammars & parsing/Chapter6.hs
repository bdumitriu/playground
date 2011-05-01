import Prelude hiding (Left, Right)
import Stack
import List
import Parser

--
-- Binary trees.
--

data BinTree x = Bin (BinTree x) (BinTree x) | Leaf x
	       deriving (Eq, Ord, Show)

type BinTreeAlgebra x a = (a -> a -> a, x -> a)

foldBinTree :: BinTreeAlgebra x a -> BinTree x -> a
foldBinTree (bin, leaf) = fold
  where
  fold (Bin t u)	= bin (fold t) (fold u)
  fold (Leaf x)		= leaf x

sizeBinTree :: BinTree x -> Int
sizeBinTree = foldBinTree ((+), const 1)

--
-- Trees for matching parantheses.
--

data Parantheses = Match Parantheses Parantheses
		 | Empty

type ParanthesesAlgebra m = (m -> m -> m, m)

foldParantheses :: ParanthesesAlgebra m -> Parantheses -> m
foldParantheses (match, empty) = fold
  where
  fold (Match t u)	= match (fold t) (fold u)
  fold Empty		= empty

depthParanthesesAlgebra :: ParanthesesAlgebra Int
depthParanthesesAlgebra = ((\x y -> max (1+x) y), 0)

widthParanthesesAlgebra :: ParanthesesAlgebra Int
widthParanthesesAlgebra = ((\_ y -> 1 + y), 0)

a2cParanthesesAlgebra :: ParanthesesAlgebra String
a2cParanthesesAlgebra = ((\x y -> "(" ++ x ++ ")" ++ y), "")

a2cParantheses :: Parantheses -> String
a2cParantheses = foldParantheses a2cParanthesesAlgebra

depthParantheses, widthParantheses :: Parantheses -> Int
depthParantheses = foldParantheses depthParanthesesAlgebra
widthParantheses = foldParantheses widthParanthesesAlgebra

paranthesesExample :: Parantheses
paranthesesExample = Match (Match (Match Empty Empty) Empty) (Match Empty (Match (Match Empty Empty) Empty))

--
-- Expression trees.
--

data E = E1 T | E2 E T
data T = T1 F | T2 T F
data F = F1 E | F2 Int

type EAlgebra e t f = ((t -> e, e -> t -> e)
		      ,(f -> t, t -> f -> t)
		      ,(e -> f, Int -> f))

foldE :: EAlgebra e t f -> E -> e
foldE ((e1, e2), (t1, t2), (f1, f2)) = fold
  where
  fold (E1 t)		= e1 (foldT t)
  fold (E2 e t)		= e2 (fold e) (foldT t)
  foldT (T1 f)		= t1 (foldF f)
  foldT (T2 t f)	= t2 (foldT t) (foldF f)
  foldF (F1 e)		= f1 (fold e)
  foldF (F2 x)		= f2 x

evalE :: E -> Int
evalE = foldE ((id, (+)), (id, (*)), (id, id))

exE :: E
exE = E2 (E1 (T2 (T1 (F2 2)) (F1 (E2 (E1 (T1 (F2 6))) (T1 (F2 3)))))) (T1 (F2 1))

a2cE :: E -> String
a2cE = foldE ((e1, e2),  (t1, t2), (f1, f2))
  where
  e1 x		= x
  e2 x y	= x ++ "+" ++ y
  t1 x		= x
  t2 x y	= x ++ "*" ++ y
  f1 x		= "(" ++ x ++ ")"
  f2 x		= show x

--
-- General trees
--

data Tree x = Node x [Tree x]

type TreeAlgebra x a = x -> [a] -> a

foldTree :: TreeAlgebra x a -> Tree x -> a
foldTree node = fold
  where
  fold (Node x ts) = node x (map fold ts)

sumTree :: Tree Int -> Int
sumTree = foldTree (\x ts -> x + sum ts)

--
-- Exercise 6.1
--
data LNTree a b = Leaf_ a
		| Node_ (LNTree a b) b (LNTree a b)

type LNTreeAlgebra a b r = (a -> r, r -> b -> r -> r)

foldLNTree :: LNTreeAlgebra a b r -> LNTree a b -> r
foldLNTree (leaf, node) = fold
  where
  fold (Leaf_ x)		= leaf x
  fold (Node_ t x u)	= node (fold t) x (fold u)

--
-- Exercise 6.2
--
-- data BinTree x = Bin (BinTree x) (BinTree x) | Leaf x
--
exBinTree :: BinTree Int
exBinTree = Bin (Bin (Bin (Bin (Leaf 2) (Leaf 7)) (Leaf 4)) (Bin (Leaf 6) (Leaf 57))) (Bin (Leaf 9) (Leaf 10))

heightBinTree :: BinTree a -> Int
heightBinTree = foldBinTree (\x y -> max x y + 1, const 0)

flatten :: BinTree a -> [a]
flatten = foldBinTree ((++), \x -> [x])

maxBinTree :: Ord a => BinTree a -> a
maxBinTree = foldBinTree (max, id)

sp :: BinTree a -> Int
sp = foldBinTree (\x y -> 1 + min x y, const 0)

mapBinTree :: (a -> b) -> BinTree a -> BinTree b
mapBinTree f = foldBinTree (Bin, Leaf . f)

--
-- Exercise 6.3
--
data Direction = Left | Right
	       deriving (Eq, Ord, Show)

type Path = [Direction]

allPaths :: BinTree a -> [Path]
allPaths (Leaf _)	= [[]]
allPaths (Bin u v)	= map (Left:) uu ++ map (Right:) vv
  where
  uu = allPaths u
  vv = allPaths v

allPaths2 :: BinTree a -> [Path]
allPaths2 = foldBinTree ((\x y -> map (Left:) x ++ map (Right:) y), const [[]])

--
-- Exercise 6.4
--
data Resistance = Capacity Float
		| Resistance :|: Resistance
		| Resistance :*: Resistance
		deriving (Eq, Ord, Show)

type ResistanceAlgebra x = (Float -> x, x -> x -> x, x -> x -> x)

foldResistance :: ResistanceAlgebra x -> Resistance -> x
foldResistance (cap, par, sequ) = fold
  where
  fold (Capacity x)	= cap x
  fold (u :|: v)	= par (fold u) (fold v)
  fold (u :*: v)	= seq (fold u) (fold v)

capacity :: Resistance -> Float
capacity = foldResistance (id, par, (+))
  where
  par r1 r2	= r1 * r2 / (r1 + r2)

--
-- Expressions.
--
infixl 7 `Mul`
infix  7 `Dvd`
infixl 6 `Add`, `Min`

type Env name value = [(name, value)]

type Name	= String
type Value	= Float

(?) :: Eq name => Env name value -> name -> value
env ? x = head [v | (y,v) <- env, x == y]

data Expr = Expr `Add` Expr
	  | Expr `Min` Expr
	  | Expr `Mul` Expr
	  | Expr `Dvd` Expr
	  | Num Value
	  | Var Name
	  | Def Name Expr Expr

type ExprAlgebra a = (a -> a -> a		-- add
		     ,a -> a -> a		-- min
		     ,a -> a -> a		-- mul
		     ,a -> a -> a		-- dvd
		     ,Value -> a		-- num
		     ,Name -> a			-- var
		     ,Name -> a -> a -> a)	-- def

foldExpr :: ExprAlgebra a -> Expr -> a
foldExpr (add, min, mul, dvd, num, var, def) = fold
  where
  fold (expr1 `Add` expr2)	= fold expr1 `add` fold expr2
  fold (expr1 `Min` expr2)	= fold expr1 `min` fold expr2
  fold (expr1 `Mul` expr2)	= fold expr1 `mul` fold expr2
  fold (expr1 `Dvd` expr2)	= fold expr1 `dvd` fold expr2
  fold (Num n)			= num n
  fold (Var x)			= var x
  fold (Def x value body)	= def x (fold value) (fold body)

resultExpr :: Expr -> (Env Name Value -> Value)
resultExpr = foldExpr ((<+>), (<->), (<*>), (</>), const, flip (?), (<:=>))
  where
  e1 <+> e2 = \env -> e1 env + e2 env
  e1 <-> e2 = \env -> e1 env - e2 env
  e1 <*> e2 = \env -> e1 env * e2 env
  e1 </> e2 = \env -> e1 env / e2 env
  x <:=> e1 = \e2 env -> e2 ((x, e1 env) : env)

seconds :: Expr
seconds = Def "days_per_year" (Num 365) (
	  Def "hours_per_day" (Num 24) (
	  Def "minutes_per_hour" (Num 60) (
	  Def "seconds_per_minute" (Num 60) (
		Var "days_per_year" `Mul`
		Var "hours_per_day" `Mul`
		Var "minutes_per_hour" `Mul`
		Var "seconds_per_minute"))))

data MachInstr v = Push v
		 | Apply (v -> v -> v)

type StackMachine v = [MachInstr v]

execute :: MachInstr v -> Stack v -> Stack v
execute (Push x) st	= push x st
execute (Apply f) st	= let a = top st
			      t = pop st
			      b = top t
			      u = pop t
			  in push (f a b) u

run :: StackMachine v -> Stack v -> Stack v
run [] st	= st
run (x:xs) st	= run xs (execute x st)

compileExpr :: Expr -> Env Name (StackMachine Value) -> StackMachine Value
compileExpr = foldExpr (add, min, mul, dvd, num, var, def)
  where
  add e1 e2	= \env -> e1 env ++ e2 env ++ [Apply (+)]
  min e1 e2	= \env -> e1 env ++ e2 env ++ [Apply (-)]
  mul e1 e2	= \env -> e1 env ++ e2 env ++ [Apply (*)]
  dvd e1 e2	= \env -> e1 env ++ e2 env ++ [Apply (/)]
  num n		= \_ -> [Push n]
  var x		= \env -> env ? x
  def x v b	= \env -> b ((x, v env):env)

resultExprStack :: Expr -> Value
resultExprStack e = top (run (compileExpr e []) emptyStack)

--
-- Exercise 6.5
--
isSum :: Expr -> Bool
isSum = foldExpr (add, min, mul, dvd, num, var, def)
  where
  add e1 e2	= e1 && e2
  min _ _	= False
  mul _ _	= False
  dvd _ _	= False
  num _		= True
  var _		= True
  def _ e1 e2	= e1 && e2

vars :: Expr -> [Name]
vars = nub . foldExpr ((++), (++), (++), (++), const [], \x -> [x], \x e1 e2 -> x : (e1 ++ e2))

--
-- Exercise 6.6
--
der :: Expr -> String -> Expr
der (e1 `Add` e2) dx	= der e1 dx `Add` der e2 dx
der (e1 `Min` e2) dx	= der e1 dx `Min` der e2 dx
der (e1 `Mul` e2) dx	= e1 `Mul` (der e2 dx) `Add` (der e1 dx) `Mul` e2
der (e1 `Dvd` e2) dx	= (e2 `Mul` (der e1 dx) `Min` e1 `Mul` (der e2 dx)) `Dvd` (e2 `Mul` e2)
der (Num f) _		= Num 0.0
der (Var x) dx		= if (x == dx) then Num 1.0 else Num 0.0
der (Def x e1 e2) dx	= error "symbolic derivation not defined on expressions with local variable definitions"

data Exp = Exp `Plus` Exp
	 | Exp `Sub` Exp
	 | Con Float
	 | Idfr Name

type ExpAlgebra a = (a -> a -> a	-- add
		     ,a -> a -> a	-- min
		     ,Value -> a	-- num
		     ,Name -> a)	-- var

foldExp :: ExpAlgebra a -> Exp -> a
foldExp (plus, sub, con, idfr) = fold
  where
  fold (exp1 `Plus` exp2)	= plus (fold exp1) (fold exp2)
  fold (exp1 `Sub` exp2)	= sub (fold exp1) (fold exp2)
  fold (Con n)			= con n
  fold (Idfr x)			= idfr x

derExp :: Exp -> String -> Exp
derExp = foldExp (plus, sub, con, idfr)
  where
  plus e1 e2	= \dx -> e1 dx `Plus` e2 dx
  sub e1 e2	= \dx -> e1 dx `Sub` e2 dx
  con f		= \_ -> Con 0.0
  idfr x	= \dx -> if (x == dx) then Con 1.0 else Con 0.0

--
-- Exercise 6.7
--
replace :: BinTree a -> a -> BinTree a
replace = foldBinTree (\t u -> \m -> Bin (t m) (u m), \x -> \m -> Leaf m)

--
-- Exercise 6.8
--
path2Value :: BinTree a -> Path -> a
path2Value = foldBinTree (bin, leaf)
  where
  bin t u	= \(p:ps) -> if p == Left then t ps else u ps
  leaf x	= \[] -> x

--
-- Block structured language
--

type Block	= [Statement]

data Statement	= Dcl Idf
		| Use Idf
		| Blk Block
		deriving (Eq, Ord, Show)

type Idf	= String

type BlockAlgebra b s = ((s -> b -> b, b),
			 (Idf -> s, Idf -> s, b -> s))

foldBlock :: BlockAlgebra b s -> Block -> b
foldBlock ((cons, empty), (dcl, use, blk)) = fold
  where
  fold (x:xs)	= cons (foldS x) (fold xs)
  fold []	= empty
  foldS (Dcl x)	= dcl x
  foldS (Use x)	= use x
  foldS (Blk b)	= blk (fold b)

type Count = Int
type Level = Int

type Variable	= (Level, Count)
type BlockInfo	= (Level, Count)

data Instruction = Enter BlockInfo
		 | Leave BlockInfo
		 | Access Variable
		 deriving (Eq, Ord, Show)

type Code = [Instruction]

--type BlockEnv	= [(Idf, Variable)]
type BlockEnv	= Env Idf Variable
type GlobalEnv	= (BlockEnv, Level)
type LocalEnv	= (BlockEnv, Count)

block2Code :: Block -> GlobalEnv -> LocalEnv -> (LocalEnv, Code)
block2Code = foldBlock ((cons, empty), (dcl, use, blk))
  where
  cons s b	= \ge le		-> let (les, cds) = s ge le
					       (leb, cdb) = b ge les
					   in (leb, cds ++ cdb)
  empty		= \ge le		-> (le, [])
  dcl x		= \(ge, l) (le, c)	-> (((x, (l, c)) : le, c+1), [])
  use x		= \(ge, l) le		-> (le, [Access (ge ? x)])
  blk b		= \(ge, l) (le, c)	-> let ((leb, cb), cdb) = b (leb, l+1) (ge, 0)
					   in ((le, c), [Enter (l+1, cb)] ++ cdb ++ [Leave (l+1, cb)])

ab2ac :: Block -> Code
ab2ac b = [Enter (0, c)] ++ cdb ++ [Leave (0, c)]
  where
  ((e, c), cdb) = block2Code b (e, 0) ([], 0)

aBlock = [Use "x", Dcl "x", Blk [Use "z", Use "y", Dcl "x", Dcl "z", Use "x"], Dcl "y", Use "y"]

--
-- Exercise 6.9
--
data Pal = Nil | LeafA | LeafB | TwoA Pal | TwoB Pal
	 deriving (Eq, Ord, Show)

type PalAlgebra a = (a, a, a, a -> a, a -> a)

foldPal :: PalAlgebra a -> Pal -> a
foldPal (nil, la, lb, ta, tb) = fold
  where
  fold Nil	= nil
  fold LeafA	= la
  fold LeafB	= lb
  fold (TwoA x)	= ta (fold x)
  fold (TwoB x)	= tb (fold x)

idPal :: PalAlgebra Pal
idPal = (Nil, LeafA, LeafB, TwoA, TwoB) 

a2cPal :: Pal -> String
a2cPal = foldPal ("", "a", "b", \x -> "a" ++ x ++ "a", \x -> "b" ++ x ++ "b")

aCountPal :: Pal -> Int
aCountPal = foldPal (0, 1, 0, (+2), id)

pFoldPal :: PalAlgebra p -> Parser Char p
pFoldPal (pal1, pal2, pal3, pal4, pal5) = pPal
  where
  pPal = (\_ x _ -> pal4 x) <$> symbol 'a' <*> pPal <*> symbol 'a'
	<|> (\_ x _ -> pal5 x) <$> symbol 'b' <*> pPal <*> symbol 'b'
	<|> const pal2 <$> symbol 'a'
	<|> const pal3 <$> symbol 'b'
	<|> const pal1 <$> succeed ""

parsePal :: String -> Pal
parsePal s = if pFoldPal idPal s == [] then error "no parse" else fst . head $ (pFoldPal idPal s)

--
-- Exercise 6.10
--
data Mir = NilM | Twoa Mir | Twob Mir
	 deriving (Eq, Ord, Show)

type MirAlgebra a = (a, a -> a, a -> a)

foldMir :: MirAlgebra a -> Mir -> a
foldMir (nil, ta, tb) = fold
  where
  fold NilM	= nil
  fold (Twoa x)	= ta (fold x)
  fold (Twob x)	= tb (fold x)

idMir :: MirAlgebra Mir
idMir = (NilM, Twoa, Twob)

a2cMir :: Mir -> String
a2cMir = foldMir ("", \x -> "a" ++ x ++ "a", \x -> "b" ++ x ++ "b")

m2pMir :: Mir -> Pal
m2pMir = foldMir (Nil, TwoA, TwoB)

pFoldMir :: MirAlgebra p -> Parser Char p
pFoldMir (mir1, mir2, mir3) = pMir
  where
  pMir = (\_ x _ -> mir2 x) <$> symbol 'a' <*> pMir <*> symbol 'a'
	<|> (\_ x _ -> mir3 x) <$> symbol 'b' <*> pMir <*> symbol 'b'
	<|> const mir1 <$> succeed ""

parseMir :: String -> Mir
parseMir s = if pFoldMir idMir s == [] then error "no parse" else fst . head $ (pFoldMir idMir s)

--
-- Exercise 6.11
--
data Parity = NilP | ZeroBefore Parity | ZeroAfter Parity | Two1 Parity

type ParityAlgebra a = (a, a -> a, a -> a, a -> a)

foldParity :: ParityAlgebra a -> Parity -> a
foldParity (nil, zb, za, to) = fold
  where
  fold NilP		= nil
  fold (ZeroBefore x)	= zb (fold x)
  fold (ZeroAfter x)	= za (fold x)
  fold (Two1 x)		= to (fold x)

a2cParity :: Parity -> String
a2cParity = foldParity ("", ("0" ++), (++ "0"), \x -> "1" ++ x ++ "1")

--
-- Exercise 6.12
--
data BitList = ConsB Bit Z | SingleB Bit
	     deriving (Eq, Ord, Show)
data Z = ConsBL BitList Z | SingleBL BitList
       deriving (Eq, Ord, Show)
data Bit = Bit0 |  Bit1
	 deriving (Eq, Ord, Show)

type BitListAlgebra bl z b = ((b -> z -> bl, b -> bl), (bl -> z -> z, bl -> z), (b, b))

foldBitList :: BitListAlgebra bl z b -> BitList -> bl
foldBitList ((consb, singleb), (consbl, singlebl), (bit0, bit1)) = fold
  where
  fold (ConsB x y)	= consb (foldB x) (foldZ y)
  fold (SingleB x)	= singleb (foldB x)
  foldZ (ConsBL x y)	= consbl (fold x) (foldZ y)
  foldZ (SingleBL x)	= singlebl (fold x)
  foldB Bit0		= bit0
  foldB Bit1		= bit1

idBitList :: BitListAlgebra BitList Z Bit
idBitList = ((ConsB, SingleB), (ConsBL, SingleBL), (Bit0, Bit1))

a2cBitList :: BitList -> String
a2cBitList = foldBitList (((\x y -> x ++ "," ++ y), id), ((\x y -> x ++ "," ++ y), id), ("0", "1"))

pFoldBitList :: BitListAlgebra bl z b -> Parser Char bl
pFoldBitList ((consb, singleb), (consbl, singlebl), (bit0, bit1)) = pBitList
  where
  pBitList = consb <$> pBit <*> pZ
	<|> singleb <$> pBit
  pZ = (\_ x y -> consbl x y) <$> symbol ',' <*> pBitList <*> pZ
	<|> (\_ x -> singlebl x) <$> symbol ',' <*> pBitList
  pBit = const bit0 <$> symbol '0'
	<|> const bit1 <$> symbol '1'

parseBitList :: String -> BitList
parseBitList s = if pFoldBitList idBitList s == [] then error "no parse" else fst . head $ (pFoldBitList idBitList s)

--
-- Exercise 6.13
--
