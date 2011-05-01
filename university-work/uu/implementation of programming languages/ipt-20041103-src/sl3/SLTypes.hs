{-# LINE 6 "SLTypes.lhs" #-}
module SLTypes where

import UU.Pretty
import Char

data Type = 
  TAny
  | TBool
  | TInt
  | TUnit
  | TFunc Type Type 
  | TForall Int Type 
  | TBound VarName
  | TVar VarName
  | TError String deriving Eq

type VarName = Int

instance Show Type where
  show (TAny)          = "ANYTYPE"
  show (TBool)         = "Bool"
  show (TInt)          = "Int"
  show (TUnit)         = "()"
  show (TFunc f a)     = "(" ++ show f ++ ")" ++ " -> " ++ show a 
  show (TForall i ts)  = show ts
  show (TBound i)      = [chr $ i + (ord 'a')]
  show (TVar v)        = "v_" ++ show v
  show (TError e)      = "<!ERROR: " ++ e ++ " !>"

type Types = [Type]
type Ids   = [String]

-- 

booltype  = TBool
inttype   = TInt
unittype  = TUnit
anytype   = TAny
arrow     = TFunc

-- 
bool2bool2bool  = booltype `arrow` (booltype `arrow` booltype)
int2int2bool    = inttype `arrow` (inttype `arrow` booltype)
int2int2int     = inttype `arrow` (inttype `arrow` inttype)

makeFnType f x = TFunc f x

getErrorMsg :: Type -> (Type, PP_Doc)
getErrorMsg (TError s)  = (TAny, ppErr (text s))
getErrorMsg t           = (t,empty)

ppErr msg = text "<!ERROR:" >#< msg >|< text "!>"

getResultType (TFunc a b)   = b
getResultType (TError msg)  = TError msg
getResultType (TAny)        = TAny
getResultType _             = TError "not a function"

getArgType (TFunc a b)   = a
getArgType (TError msg)  = TError msg
getArgType (TAny)        = TAny
getArgType _             = TError "not a function"

getOpErrors (TError s, TError t)  = (ppErr (text s), ppErr (text t), True)
getOpErrors (TError s, _)         = (ppErr (text s), empty, True)
getOpErrors (_, TError s)         = (empty, ppErr (text s), True)
getOpErrors (TAny, _)             = (empty, empty, True)
getOpErrors (_, TAny)             = (empty, empty, True) 
getOpErrors _                     = (empty,empty, False)

isAnnotated (TAny)  = False
isAnnotated _       = True
