{-# LINE 6 "SLTypes.lhs" #-}
module SLTypes where

data Type = TAny
	  | TBool
	  | TInt
	  | TUnit
	  | TFunc Type Type
	  | TTuple [Type]	-- added a new constructor for representing the
				-- tuple type. The list of Type's is used for
				-- storing the type of each element of the
				-- tuple, thus allowing n-tuples, with n being a
				-- random number.

instance Show Type where
	show (TAny)        = "ANYTYPE"
	show (TBool)       = "Bool"
	show (TInt)        = "Int"
	show (TUnit)       = "()"
	show (TFunc f a)   = show f ++ " -> " ++ show a
	show (TTuple list) = "(" ++ concat (sepWith (map show list) ",") ++ ")"
				-- added code which is used for displaying a
				-- tuple type.

--
-- Given a list and an separator, returns a the list obtained by separating all
-- elements in the original list using the separator.
--
sepWith :: [a] -> a -> [a]
sepWith [] _		= []
sepWith [x] _		= [x]
sepWith (x:xs) y	= x : y : sepWith xs y

type Types = [Type]

-- 

booltype        = TBool
inttype         = TInt
unittype        = TUnit
anytype         = TAny
makeFnType f x  = TFunc f x
tupleType tlist = TTuple tlist	-- added a "wrapper" function for generating a
				-- tuple type.

isAnnotated TAny  = False
isAnnotated _     = True
