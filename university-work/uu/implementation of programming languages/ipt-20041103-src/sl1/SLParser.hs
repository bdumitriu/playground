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
                   "let", "in", "ni",
                   "True", "False",
                   "Int", "Bool", "Unit"
                 ]
keywordsOps   =  [ "=", "\\", "->", "::",":=",
                   "&&", "||",
                   "==", "/=","<", ">","<=",">=",
                   "+","-","*", "/",
                   "!!"			-- new operator for accessing members
					-- of a tuple
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
  pAssignOrExpr    =  sem_Expr_Assign <$> pPattern <* pKey ":=" <*> pExpr
                        <|> pExpr
  pExpr            =  pAndPrioExpr
                        <|> pLambda

  -- Changed this in order to allow pattern matching of the arguments of a lambda function.
  -- Also, changed the definition of Vars, so sem_Vars_Cons and sem_Vars_Nil now support
  -- pattern matching.

  pLambda          =  sem_Expr_Lam <$
                        pKey "\\"  <*> pFoldr1 (sem_Vars_Cons, sem_Vars_Nil) pPattern
                                   <* pKey "->"  <*> pExpr
  pAndPrioExpr     =  pChainl  (sem_Expr_Op <$> pOps [ "&&", "||"])
                               pCmpPrioExpr
  pCmpPrioExpr     =  pChainl  (sem_Expr_Op <$> pOps ["==", "/=","<", ">","<=",">="])
                               pAddPrioExpr
  pAddPrioExpr     =  pChainl  (sem_Expr_Op <$> pOps [ "+","-"])
                               pMulPrioExpr
  pMulPrioExpr     =  pChainl  (sem_Expr_Op <$> pOps [ "*", "/"])
                               pSelPrioExpr
  pSelPrioExpr     =  pChainl  (sem_Expr_Op <$> pOps [ "!!" ])	-- added the !! operator in the chain of operators
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

                        -- Added an option for parsing a n-tuple, with n being a random number. Instead of
                        -- using a mere (... <$> pParens (pCommas pExpr)) I have used the more complicated
                        -- parser below in order to make sure that we only parse something which has at
                        -- least 2 entries as a tuple (i.e. that we do not parse () and (val) as tuples).

                        <|> pParens
                          ((\e es -> sem_Expr_Tupleexpr (foldr sem_TupleExprs_Cons sem_TupleExprs_Nil (e:es)))
                          <$> pExpr <* pComma <*> pList1Sep pComma pExpr)

                        <|> pParens pExpr
                        <|> sem_Expr_If
                          <$ pKey "if"    <*> pExpr
                          <* pKey "then"  <*> pExpr
                          <* pKey "else"  <*> pExpr
                          <* pKey "fi"
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

                        -- Added an option for parsing a n-tuple type (i.e. the type definition for
                        -- a tuple). Although placing this option after the parser for () and the
                        -- the parser for (type) ensures that no tuple type with less than two values
                        -- is parsed as a first option, I have also included an additional safety
                        -- measure by means of the parser itself (similar to the case of parsing tuple
                        -- values above) so that such a tuple type (one with less than two values)
                        -- wouldn't be successfully parsed as a second option either.

                        <|> pParens ((\x xs -> tupleType (x:xs)) <$> pType <* pComma <*> pList1Sep pComma pType)
  
  -- Parser either for single variable names or for a (possibly nested) pattern (such as (a,b,(c,d)).
  -- This parser is used in pLambda for pattern matching.
  
  pPattern         =  sem_Pattern_Single <$> pVarid
                        <|> (\ps -> sem_Pattern_Tuple (foldr sem_Patterns_Cons sem_Patterns_Nil ps))
                             <$> pParens (pCommas pPattern)

string2int = foldl (\val dig -> (10*val + ord dig -ord '0')) 0
