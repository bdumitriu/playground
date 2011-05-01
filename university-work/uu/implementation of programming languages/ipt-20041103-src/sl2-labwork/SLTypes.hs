{-# LINE 6 "SLTypes.lhs" #-}
module SLTypes where

import UU.Pretty

data Type = TAny
  | TBool
  | TInt
  | TUnit
  | TFunc Type Type deriving Eq


instance Show Type where
  show (TAny)       = "ANY"
  show (TBool)      = "Bool"
  show (TInt)       = "Int"
  show (TUnit)      = "()"
  show (TFunc f a)  = "(" ++ show f ++ ")" ++ " -> " ++ show a 
  

type Types  = [Type]
type Ids    = [String]

booltype  = TBool
inttype   = TInt
unittype  = TUnit
anytype   = TAny

ppErr msg = text "<!ERROR:" >#< msg >|< text "!>"

makeFnType f x = TFunc f x

matchTypes TAny t     =  (t,empty)
matchTypes t TAny     =  (t,empty)
matchTypes t expType  =  if t == expType 
                         then (t,empty) 
                         else (anytype, ppErr ("Expected " ++ show expType))

checkIfLambda tp = case tp of
  (TFunc f x)       -> (f,x,empty) 
  TAny              -> (anytype, 
                         foldr makeFnType anytype (repeat anytype), 
                         empty)
  _                 -> (anytype, 
                         foldr makeFnType anytype (repeat anytype), 
                         ppErr "Is not a function")

getFirstArgType []      = (anytype, repeat anytype)
getFirstArgType (a:as)  = (a,as) 

codeSize :: Type -> Int
codeSize (TUnit)      = 0
codeSize (TInt)       = 1
codeSize (TBool)      = 1
codeSize (TFunc f x)  = 2
codeSize _            = 0

extractResType (TFunc _ (TFunc a b))   = extractResType (TFunc a b)
extractResType (TFunc a b)             = b
extractResType _                       = anytype

extractArgTypes (TFunc a (TFunc b c))  = a : extractArgTypes (TFunc b c)
extractArgTypes (TFunc a b)            = [a] 
extractArgTypes  _                     = replicate 30 anytype

makeFunctionType argTypes bodyTp = foldr makeFnType bodyTp argTypes

ifLambda tp = case tp of
  (TFunc _ _) -> True
  _           -> False   

isAnnotated TAny  = False
isAnnotated _     = True
