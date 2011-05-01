module PropDD (
	Prop(..),
	Assoc(..),
	Oper(..),
	UnaryOperator(..),
	BinaryOperator(..),
	mk_TT, mk_FF, mk_Ident, mk_Binary, mk_NOT
	)

where

{--
A proposition formula has one of the following form:

  * a variable
  * constant true or false
  * p /\ q			conjunction (AND operator)
  * p \/ q			disjunction (OR operator)
  * p ==> q			implication
  * p <=> q			bi-implication
  * ~p				negation of p

Replace the definition of the type Prop below so that you can
represent the above kinds of proposition formulas.  
--}

--
-- This class defines a few functions specific to operators. I have defined it in order
-- to be able to handle various decisions about operators at a more abstract level. The
-- use of this class also allows the possiblity of very easily extending the set of
-- operators without rewriting the all of code.
--
class Oper a where
  symbol :: a -> String
  priority :: a -> Int
  assoc :: a -> Assoc

data Prop = Var String
	  | Val Bool
	  | Un UnaryOperator Prop
	  | Bin BinaryOperator Prop Prop
          deriving (Eq, Show, Ord)

--
-- The Prop data type is actually not a real instance of the Oper class. It is however defined
-- like this (sort of a pseudo Oper) in order to allow the simplification of the code in various
-- functions. As defined below, Prop actually returns values for its top-most operator. For
-- example, if x = (Conj (Disj p q) r), all the Oper functions for x will actually return the
-- values for Conj, as Conj is the top-most operator in x. Also, some default definitions are
-- given for the Var and Val constructors.
--
instance Oper Prop where
  symbol (Var _)		= ""
  symbol (Val _)		= ""
  symbol (Un oper _)		= symbol oper
  symbol (Bin oper _ _)		= symbol oper
  priority (Var _)		= 0
  priority (Val _)		= 0
  priority (Un oper _)		= priority oper
  priority (Bin oper _ _)	= priority oper
  assoc (Var _)			= Undefined
  assoc (Val _)			= Undefined
  assoc (Un oper _)		= assoc oper
  assoc (Bin oper _ _)		= assoc oper

--
-- Data type for possible associativities of operators. The Undefined value is to be used for
-- all non-binary operators (as associativity can only be defined for binary operators).
--
data Assoc = RightA | LeftA | NonA | Undefined
	   deriving (Eq, Show, Ord)

--
-- Data type for all unary operators. Of course, with just one unary operator in use, one might
-- argue that it's a bit of an overkill, but it might prove useful, were we to later introduce
-- some additional operators.
--
data UnaryOperator = Neg
		   deriving (Eq, Show, Ord)

--
-- Define UnaryOperator as an instance of Oper.
--
instance Oper UnaryOperator where
  symbol Neg	= "~"
  priority Neg	= 1
  assoc Neg	= Undefined

--
-- Data type for all binary operators.
--
data BinaryOperator = Conj | Disj | Impl | Equiv
		    deriving (Eq, Show, Ord)

--
-- Define BinaryOperator as an instance of Oper.
--
instance Oper BinaryOperator where
  symbol Conj		= "/\\"
  symbol Disj		= "\\/"
  symbol Impl		= "==>"
  symbol Equiv		= "<=>"
  priority Conj		= 2
  priority Disj		= 3
  priority Impl		= 4
  priority Equiv	= 5
  assoc Conj		= LeftA
  assoc Disj		= LeftA
  assoc Impl		= RightA
  assoc Equiv		= NonA

{-
To interface with the parsers you will need to give a concrete
definition for the interface below.
-}

--
-- mk_TT makes a Prop value representing true.
--
mk_TT :: Prop
mk_TT = Val True

--
-- mk_FF makes a Prop value representing false
--
mk_FF :: Prop
mk_FF = Val False

---
-- Given a string v, mk_Ident makes a proposition formula that only consists of v.
--
mk_Ident :: String -> Prop
mk_Ident = Var

--
-- Given two formulas p and q, mk_Binary "AND" p q makes a prop value representing the formula
-- p /\ q. The same goes for "OR", "IMP", and "EQUIV".
--
mk_Binary :: String -> Prop -> Prop -> Prop
mk_Binary "AND" = Bin Conj
mk_Binary "OR" = Bin Disj
mk_Binary "IMP" = Bin Impl
mk_Binary "EQUIV" = Bin Equiv

--
-- Given a formula p, mk_NOT P makes a Prop value representing ~p.
--
mk_NOT :: Prop -> Prop
mk_NOT = Un Neg
