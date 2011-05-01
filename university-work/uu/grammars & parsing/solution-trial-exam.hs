import ParseLib

data Pred = Equal Expr Expr
	  | And Pred Pred
	  deriving (Eq, Ord, Show)

data Expr = Ident String
	  | Let String Expr Expr
	  | Plus Expr Expr
	  deriving (Eq, Ord, Show)

exSentence :: String
exSentence = "x == let w = y+z in w\n&&\nx == z"

exS2 :: String
exS2 = "x == w && y == x+y+z"

parse :: String -> Pred
parse s = if null r then error "no parse" else fst . head $ r
  where
  r = pPred s

pPred :: Parser Char Pred
pPred = chainl pEq (const . const . const And <$> option ws "" <*> token "&&" <*> option ws "")

pEq :: Parser Char Pred
pEq = (\e1 _ _ _ e2 -> Equal e1 e2) <$> pExpr <*> ws <*> token "==" <*> ws <*> pExpr

pExpr :: Parser Char Expr
pExpr = chainl pBasicExpr (const Plus <$> token "+")

pBasicExpr :: Parser Char Expr
pBasicExpr = Ident <$> pIdentifier
	<|>  (\_ _ x _ _ _ y _ _ _ z -> Let x y z) <$>
	token "let"
	<*> ws
	<*> pIdentifier
	<*> ws
	<*> token "="
	<*> ws
	<*> pExpr
	<*> ws
	<*> token "in"
	<*> ws
	<*> pExpr

pIdentifier :: Parser Char String
pIdentifier = token "w" <|> token "x" <|> token "y" <|> token "z"

ws :: Parser Char String
ws = many (symbol ' ' <|> symbol '\t' <|> symbol '\n')

type PredAlgebra e p = ((e -> e -> p, p -> p -> p), (String -> e, String -> e -> e -> e, e -> e -> e))

foldPred :: PredAlgebra e p -> Pred -> p
foldPred ((p1, p2), (e1, e2, e3)) = fold
  where
  fold (Equal ex1 ex2)		= p1 (foldE ex1) (foldE ex2)
  fold (And pr1 pr2)		= p2 (fold pr1) (fold pr2)
  foldE (Ident st)		= e1 st
  foldE (Let st ex1 ex2)	= e2 st (foldE ex1) (foldE ex2)
  foldE (Plus ex1 ex2)		= e3 (foldE ex1) (foldE ex2)

testAlgebra :: PredAlgebra Bool Bool
testAlgebra = (((||), (||)), (const False, \_ _ _ -> True, (||)))

test :: Pred -> Bool
test = foldPred testAlgebra

type Env = [(String, Int)]

eval :: Pred -> Env -> Bool
eval = foldPred evalAlgebra

evalAlgebra :: PredAlgebra (Env -> Int) (Env -> Bool)
evalAlgebra = ((p1, p2), (e1, e2, e3))
  where
  p1 ex1 ex2 env	= ex1 env == ex2 env
  p2 pr1 pr2 env	= pr1 env && pr2 env
  e1 st env		= maybe (error "var not found") id (lookup st env)
  e2 st ex1 ex2 env	= ex2 ((st, ex1 env):env)
  e3 ex1 ex2 env	= ex1 env + ex2 env
