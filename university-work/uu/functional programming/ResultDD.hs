module ResultDD 

where

data Result a b =
	  Success a
	| Fail	  b
	deriving (Eq,Show)