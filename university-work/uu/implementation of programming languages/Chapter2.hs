module Chapter2 where

import UU.Parsing
import Char

instance Symbol Char

on p inp
	= do	{ res <- parseIO p inp
		; putStr ("Result:\n" ++ show res ++ "\n")
		}

main :: IO ()
main
	= do	{ putStr "Enter expression: "
		;  inp <- getLine
		;  if inp /= ""  then do	{ pExprPCM `on` inp
						; main
						}
		   else putStr "done\n"
		}

--
-- Exercise 2.1
--

pExprPC :: (IsParser p Char) => p Int
pExprPC = let	pParens p	= pSym '(' *> p <* pSym ')'
		pDigit		= (\d -> ord d - ord '0') <$> pAnySym ['0' .. '9']
		pNat		= foldl (\a b -> a * 10 + b) 0 <$> pList1 pDigit
		pFactor		= (pSym '+' `opt` ' ') *> pNat
				<|> (\x -> -x) <$ pSym '-' <*> pNat
				<|> pParens pExpr
		pTerm		= pChainr ((*) <$ pSym '*' <|> div <$ pSym '/') pFactor
		pExpr		= pChainr ((+) <$ pSym '+' <|> (-) <$ pSym '-') pTerm
	  in pExpr

--
-- Exercise 2.2
--

pExprPCRPN :: (IsParser p Char) => p String
pExprPCRPN = let	pParens p	= pSym '(' *> p <* pSym ')'
			binOper s x y	= x ++ y ++ s ++ "\n"
			unOper s x	= x ++ "\n" ++ (if s /= "" then s ++ "\n" else "")
			pDigit		= pAnySym ['0' .. '9']
			pNat		= pList1 pDigit
			pFactor		= (\x -> unOper "" x) <$ (pSym '+' `opt` ' ') <*> pNat
					<|> (\x -> unOper "~" x) <$ pSym '-' <*> pNat
					<|> pParens pExpr
			pTerm		= pChainr ((\s -> binOper [s]) <$> (pSym '*' <|> pSym '/')) pFactor
			pExpr		= pChainr ((\s -> binOper [s]) <$> (pSym '+' <|> pSym '-')) pTerm
	     in pExpr

--
-- Exercise 2.3
--

type Env = [(String, Int)]

(?) :: Env -> String -> Int
[] ? _		= error "variable not bound"
((x,y):env) ? z	= if x == z then y else env ? z

pExprPCM :: (IsParser p Char) => p [Int]
pExprPCM
	= let	pParens p	= pSym '(' *> p <* pSym ')'
		pSemics p	= pListSep (pSym ';') p
		pDigit		= (\d -> ord d - ord '0') <$> pAnySym ['0' .. '9']
		pNat		= foldl (\a b -> a * 10 + b) 0 <$> pList1 pDigit
		pChar		= pAnySym (['a' .. 'z'] ++ ['A' .. 'Z'])
		pNum		= pAnySym ['0' .. '9']
		pStart		= pChar <|> pSym '_'
		pIdentifier	= (:) <$> pStart <*> pList (pNum <|> pChar <|> pSym '_')
		--pFactor :: (IsParser p Char) => p (Env -> (Int, Env))
		pFactor		= (\x env -> (x, env)) <$ (pSym '+' `opt` ' ') <*> pNat
				<|> (\x env -> (-x, env)) <$ pSym '-' <*> pNat
				<|> (\x env -> (env ? x, env)) <$> pIdentifier
				<|> pParens pExpr
		--pTerm :: (IsParser p Char) => p (Env -> (Int, Env))
		pTerm		= pChainr ((\f1 f2 env -> let (x, _) = f1 env
							      (y, _) = f2 env
							  in (x * y, env)) <$ pSym '*'
				       <|> (\f1 f2 env -> let (x, _) = f1 env
							      (y, _) = f2 env
 							  in (x `div` y, env)) <$ pSym '/') pFactor
		--pExpr :: (IsParser p Char) => p (Env -> (Int, Env))
		pExpr		= pChainr ((\t1 t2 env -> let (x, _) = t1 env
							      (y, _) = t2 env
							  in (x + y, env)) <$ pSym '+'
				       <|> (\t1 t2 env -> let (x, _) = t1 env
							      (y, _) = t2 env
							  in (x - y, env)) <$ pSym '-') pTerm
		-- pAssignment :: (IsParser p Char) => p (Env -> (Int, Env))
		pAssignment	= (\var expr env -> let (val, _) = expr env in (val, (var, val):env))
				<$> pIdentifier <* pSym '=' <*> pExpr
		-- pStatement :: (IsParser p Char) => p (Env -> (Int, Env))
		pStatement	= pAssignment <|> pExpr
		-- pPCM :: (IsParser p Char) => p [Env -> (Int, Env)]
		pPCM		= pSemics pStatement
	   in (f []) <$> pPCM

f :: Env -> [Env -> (Int, Env)] -> [Int]
f _ []		= []
f env (x:xs)	= let (res, env') = x env in res : f env' xs
