{-data Tree = Red [Tree]
	  | Blue [Tree]
  deriving (Eq, Ord, Show)

--f s = Blue (map Red s)

--g (Red s) = Blue (map Red s)
--g (Blue s) = Red (map Blue s)


data Book a = Paragraph String
	    | Struct StructType a [Book a]

data StructType = Chapter | Section | Subsection
		  deriving Eq

instance (Eq a) => Eq (Book a) where
  Paragraph x == Paragraph y		= x==y
  Struct t i bs == Struct u j cs	= t==u && i==j && bs==cs

labelBook :: Book a -> Book (a,Int)
labelBook b = fst (helper b)

helper :: Book a -> (Book (a,Int), Int)
helper (Paragraph t) = (Paragraph t, 1)
helper (Struct t i bs) = (Struct t (i,n) bs', n)
  where
  (bs',counts) = unzip (map helper bs)
  n            = sum counts

--f z s = map g (map fst s)
--  where
--  g a = elem a z

--g z s = map (elem z) (map fst s)

--f x = [x]
--g x = map f

--f s = snd (maximum (zip s [0 .. ]))

--foo s = zipWith f s (repeat null) where f a b = b a

--flop x s = and (map (<x) s)

--flop2 x s = all (<x) s

data Expr a = Const a
	    | Plus (Expr a) (Expr a)
	    | Maal (Expr a) (Expr a)
	    deriving Show

flop a b c = a <= c && elem b c
-}
----------------------------------------------------------

data Expr = Empty
	  | Plus Basic Expr Decls
	  deriving (Eq, Ord, Show)

data Basic = Ident String
	   | Nested Expr
	   deriving (Eq, Ord, Show)

data Decls = Null
	   | Decls String Expr Decls
	   deriving (Eq, Ord, Show)

type ExprAlgebra b d r = ((r, b -> r -> d -> r),
			  (String -> b, r -> b),
			  (d, String -> r -> d -> d))

type Name	= String
type Var	= Int
type Env	= [(Name, Var)]

(?) :: Env -> Name -> Var
[] ? x		= error ("variable " ++ x ++ " not bound in environment!")
((n,v):xs) ? x
  | x == n	= v
  | otherwise	= xs ? x

foldExpr :: ExprAlgebra b d r -> Expr -> r
foldExpr ((fempty, fplus), (fident, fnested), (fnull, fdecls)) = fold
  where
  fold Empty		= fempty
  fold (Plus b e d)	= fplus (foldB b) (fold e) (foldD d)
  foldB (Ident x)	= fident x
  foldB (Nested e)	= fnested (fold e)
  foldD Null		= fnull
  foldD (Decls x e d)	= fdecls x (fold e) (foldD d)

evalAlgebra :: ExprAlgebra (Env -> Int) (Env -> Env) (Env -> Int)
evalAlgebra = ((fempty, fplus), (fident, fnested), (fnull, fdecls))
  where
  fempty env		= 0
  fplus b e d env	= let ue = d env in b ue + e ue
  fident st env		= env ? st
  fnested e env		= e env
  fnull env		= env
  fdecls st e d env	= (st, e env) : d env

evalExpr :: Expr -> Env -> Int
evalExpr e env = foldExpr evalAlgebra e env

exExpr :: Expr
exExpr = Plus
		(Ident "x")
		(Plus
			(Ident "y")
			Empty
			Null
		)
		(Decls
			"x"
			(Plus
				(Ident "a")
				(Plus
					(Ident "y")
					Empty
					Null
				)
				Null
			)
			(Decls
				"y"
				(Plus
					(Ident "a")
					(Plus
						(Ident "b")
						Empty
						Null
					)
					Null
				)
				Null
			)
		)
