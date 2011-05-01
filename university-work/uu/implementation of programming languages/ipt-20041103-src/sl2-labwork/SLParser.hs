{-# LINE 10 "SLParser.lhs" #-}
module SLParser where

import UU.Parsing
import UU.Scanner
import SLAttributes
import SLTypes
import Char
{-# LINE 25 "SLParser.lhs" #-}
slScan name   = scanFile keywordsText keywordsOps specialChars opChars name

keywordsText  =  [ "if", "then", "else", "fi",
                   "while", "do", "od",
                   "let", "in", "ni",
                   "True", "False",
                   "Int", "Bool", "Unit"
                 ]
keywordsOps   =  [ "=", "\\", "->", "::",":=",
                   "&&", "||",
                   "==", "/=","<", ">","<=",">=",
                   "+","-","*", "/", "%"
                 ]
specialChars  = "();,[]"
opChars       = "!#$%&*+/<=>?@\\^|-:"
{-# LINE 45 "SLParser.lhs" #-}
pRoot _ = sem_Root_Root <$> pExprSeq
  where
  pExprSeq         =  (\exprs -> case exprs of
                                  [e] -> e
                                  es  -> sem_Expr_Seq (foldr sem_Exprs_Cons sem_Exprs_Nil es))
                      <$> pSemics pAssignOrExpr
  pAssignOrExpr    =  sem_Expr_Assign <$> pVarid <* pKey ":=" <*> pExpr
                        <|> pExpr                   
  pExpr            =  pAndPrioExpr
                        <|> pLambda
  pLambda          =  sem_Expr_Lam <$ 
                        pKey "\\"  <*> pFoldr1 (sem_Vars_Cons, sem_Vars_Nil) pVarid 
                                   <* pKey "->"  <*> pExpr
  pAndPrioExpr     =  pChainl  (sem_Expr_Op <$> pOps [ "&&", "||"])
                               pCmpPrioExpr
  pCmpPrioExpr     =  pChainl  (sem_Expr_Op <$> pOps ["==", "/=","<", ">","<=",">="])
                               pAddPrioExpr
  pAddPrioExpr     =  pChainl  (sem_Expr_Op <$> pOps [ "+","-"])
                               pMulPrioExpr
  pMulPrioExpr     =  pChainl  (sem_Expr_Op <$> pOps [ "*", "/", "%"])
                               pLamApply
  pOps ops         =  pAny pKey ops
  pLamApply        =  (\fs -> case fs of
                         [fe]        -> fe
                         _           -> sem_Expr_Lamcall (foldl1 sem_Expr_Apply fs)
                      ) <$> pList1 pFactor
  pFactor          =  sem_Expr_Boolexpr True  <$ pKey "True"
                        <|> sem_Expr_Boolexpr False <$ pKey "False"
                                                <|> sem_Expr_Unit <$ pKey "Unit"
                        <|> sem_Expr_Ident <$> pVarid
                        <|> (sem_Expr_Intexpr . string2int) <$> pInteger
--			<|> (sem_Expr_Intexpr . neg . string2int) <$ pKey "-" <*> pInteger
                        <|> pParens pExpr
                        <|> sem_Expr_If
                          <$ pKey "if"    <*> pExpr
                          <* pKey "then"  <*> pExpr
                          <* pKey "else"  <*> pExpr
                          <* pKey "fi"
			<|> sem_Expr_While
			  <$ pKey "while" <*> pExpr
			  <* pKey "do" <*> pExprSeq
			  <* pKey "od"
                        <|> sem_Expr_Let
                          <$ pKey "let"  <*> pDecls
                          <* pKey "in"   <*> pExprSeq
                          <* pKey "ni"
  pDecls           =  foldr sem_Decls_Cons sem_Decls_Nil <$> pSemics pDecl
  pDecl            =  sem_Decl_Decl <$> pVarid <*> pTypeDecl <* pKey "=" <*> pExpr
  pTypeDecl        =  pKey "::" *> pType <|> pSucceed anytype
  pType            =  pChainr (makeFnType <$ pKey "->") pNonFuncType
  pNonFuncType     =  inttype <$  pKey "Int" 
                        <|> booltype <$ pKey "Bool"
                        <|> unittype <$ pKey "()"
                        <|> pParens pType

string2int = foldl (\val dig -> (10*val + ord dig -ord '0')) 0
neg n = -n
