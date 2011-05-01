{-# LINE 8 "Expr.lhs" #-}
module Expr where
import UU.Parsing

instance Symbol Char

pExpr :: IsParser a Char => a Int
pExpr
  =  let  pParens p  =  pSym '(' *> p <* pSym ')'
          pDigit     =  (\d -> ord d - ord '0')
                        <$> pAnySym ['0'..'9']
          pNat       =  foldl (\a b -> a*10 + b) 0 <$> pList1 pDigit
          pFact      =  pNat <|> pParens pExpr
          pTerm      =  pChainl ((*) <$ pSym '*'
                     <|>  div <$ pSym '/' ) pFact
          pExpr      =  pChainl ((+) <$ pSym '+'
                     <|>  (-) <$ pSym '-' ) pTerm
      in  pExpr
{-# LINE 30 "Expr.lhs" #-}
on p inp
  =  do  {  res <- parseIO p inp
         ;  putStr ("Result:\n" ++ show res ++ "\n")
         }

main :: IO ()
main
  =  do  {  putStr "Enter expression: "
         ;  inp <- getLine
         ;  if inp /= "" 
              then  do  { pExpr `on` inp
                        ; main
                        }
              else  putStr "done\n"
         }
