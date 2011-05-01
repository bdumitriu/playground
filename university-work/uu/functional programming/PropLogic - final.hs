module PropLogic

where

import PropDD
import ResultDD
import PropParser
import List
import Maybe
import Char

{------------------------------------------------------------ 
The module PropParser exports a function parseProp. With it you can
read a formula as a string, and it will convert it to a value of type
Prop (your internal representation of formulas).
------------------------------------------------------------}

example1 = parseProp "(p==>q) /\\ (q==>r) ==> (p==>r)"

{------------------------------------------------------------- 
Note: if the input is a syntactically good formula, parseProp will
return a value of the from Success z, where z is the resulting Prop
value. Else you will get a value of the form Fail something.

Alternatively, you can use xparseProp: 

xparseProp :: String -> Prop
--------------------------------------------------------------}

--
-- Examples with xparseProp.
--
example2 = xparseProp "(p==>q) /\\ (q==>r) ==> p ==> r"
example3 = xparseProp "(p /\\ p ==> q) /\\ (q ==> r \\/ r) ==> ~(~p \\/ true) ==> r"
example4 = xparseProp "(p /\\ p ==> q) /\\ (q ==> r \\/ r) ==> ~(~p \\/ false) ==> r"

--
-- Shows a proposition formula as a string.
--
showProp :: Prop -> String
showProp (Var x)	= x
showProp (Val b)	= lowerCaseBool b
showProp (Un oper p)	= symbol oper ++ decideBrackets oper p AtRight (showProp p)
showProp (Bin oper p q)	= decideBrackets oper p AtLeft (showProp p) ++
			  ' ' : symbol oper ++
			  ' ' : decideBrackets oper q AtRight (showProp q)

--
-- Returns "true" for True and "false" for False.
--
lowerCaseBool :: Bool -> String
lowerCaseBool True	= "true";
lowerCaseBool False	= "false";

--
-- Data type for indicating the position of a proposition with respect to an operator.
-- In p /\ q, p would be AtLeft of /\, while q would be AtRight of /\. Values of
-- this data type are used in order to participate in the decision about parathesising
-- or not parathesising a proposition when dealing with operators of the same priority
-- and associativity considerations have to be employed.
--
data Position = AtLeft | AtRight
	      deriving (Eq, Show, Ord)

--
-- Decides whether or not to paranthesise a proposition. Its parameters are:
--  (*) an operator (instance of a data type deriving Operator);
--  (*) a proposition, which represents the proposition at the left or at the right of the operator;
--  (*) a position, which represents the actual position of the proposition with respect to the operator;
--  (*) a String, which represents the String which is or is not to be paranthesised.
-- The function returns a String, which is the input String, either paranthesised or not.
-- (Please note that the smaller the priority _value_ for an operator, the greater its _real_ priority is)
--
decideBrackets :: Oper a => a -> Prop -> Position -> String -> String
decideBrackets oper p pos st
  | po < pp				= "(" ++ st ++ ")"
  | po > pp				= st
  -- at this point, I already know that po == pp
  | ao == LeftA && pos == AtRight	= "(" ++ st ++ ")"
  | ao == RightA && pos == AtLeft	= "(" ++ st ++ ")"
  | ao == NonA				= "(" ++ st ++ ")"
  | otherwise				= st
  where
  po = priority oper
  pp = priority p
  ao = assoc oper

--
-- Prints a formula on the screen.
--
printProp :: Prop -> IO ()
printProp = putStr . showProp


--
-- Type used to associate identifiers with values (true or false).
--
type Valuation = [(String, Bool)]

--
-- Evaluates a proposition using a given Valuation for its identifiers.
--
evaluate :: Prop -> Valuation -> Bool
evaluate (Var x) model		= maybe (error ("Variable " ++ x ++ " not mapped in valuation.")) id (lookup x model)
evaluate (Val x) _		= x
evaluate (Un Neg p) model	= not . evaluate p $ model
evaluate (Bin Conj p q) model	= (evaluate p model) && (evaluate q model)
evaluate (Bin Disj p q) model	= (evaluate p model) || (evaluate q model)
evaluate (Bin Impl p q) model	= (evaluate p model) ==> (evaluate q model)
evaluate (Bin Equiv p q) model	= (evaluate p model) == (evaluate q model)

--
-- Definition of the implication operator.
--
(==>) :: Bool -> Bool -> Bool
True ==> False	= False
_ ==> _		= True

--
-- Returns the set of all Valuations for which a proposition evaluates to true.
--
satisfySet :: Prop -> [Valuation]
satisfySet p = filter (\x -> evaluate p x == True) (generateValuations p)

--
-- Generates all possible valuations for a proposition. For example, if the proposition contains
-- two identifiers, say p and q, this function will return the following list:
-- [[("p", False), ("q", False)], [("p", True), ("q", False)], [("p", False), ("q", True)], [("p", True), ("q", True)]]
--
generateValuations :: Prop -> [Valuation]
generateValuations = generateAllValuations . extractUniqueVars

--
-- Generates all possible valuations for a list of identifiers. For example, if the initial list is
-- ["p", "q"], then this function will return the following list:
-- [[("p", False), ("q", False)], [("p", True), ("q", False)], [("p", False), ("q", True)], [("p", True), ("q", True)]]
--
generateAllValuations :: [String] -> [Valuation]
generateAllValuations = foldr f [[]]
  where
  f x		= concat . map (g x)
  g x rec	= [(x, False) : rec, (x, True) : rec]

--
-- Produces a _set_ of all the identifiers that appear in a proposition.
--
extractUniqueVars :: Prop -> [String]
extractUniqueVars = nub . extractVars

--
-- Produces a _list_ of all the identifiers that appear in a proposition.
--
extractVars :: Prop -> [String]
extractVars (Var x)	= [x]
extractVars (Val x)	= []
extractVars (Un _ p)	= extractVars p
extractVars (Bin _ p q)	= extractVars p ++ extractVars q

--
-- Decides whether or not the proposition is a tautology.
--
tautology :: Prop -> Bool
tautology p = length (satisfySet p) == 2^(length . extractUniqueVars $ p)

--
-- Decides whether or not the proposition is a contradiction.
--
contradiction :: Prop -> Bool
contradiction = null . satisfySet

--
-- Decides whether or not two propositions are equivalent.
--
equivalent :: Prop -> Prop -> Bool
equivalent p q = null . filter f $ generateAllValuations . extractAllVars p $ q
  where
  f x = evaluate p x /= evaluate q x

--
-- Produces a _set_ of all the identifiers that appear in either of the two propositions
-- it receives as parameters.
--
extractAllVars :: Prop -> Prop -> [String]
extractAllVars p = union (extractVars p) . extractVars

--
-- Type definition for representing propositions in Disjunctive Normal Form.
--
type Atom	= String
type Clause	= [Atom]
type DNF	= [Clause]

--
-- Transforms a proposition into a DNF representation.
-- Explanations:
--  (*) takeToDNF and propToDNF are explained below, where the functions are defined;
--  (*) (sortBy cmp . nub) is applied to each Atom in order to first remove duplicate identifiers
--      and then sort the list according to the order given by the comparison function cmp, defined
--      below;
--  (*) the final nub is meant to remove duplicate Atoms (this can be done using nub, since the
--      identifiers in each atom are already sorted).
--
toDNF :: Prop -> DNF
toDNF = nub . map (sortBy cmp . nub) . propToDNF . takeToDNF

--
-- Custom String comparison function which gives a special, non-lexicographic, ordering for
-- "~var" and "var".
--
cmp :: String -> String -> Ordering
cmp ('~':xs) ('~':ys)	= compare xs ys
cmp ('~':xs) y		= if (xs == y) then LT else compare xs y
cmp x ('~':ys)		= if (x == ys) then GT else compare x ys
cmp x y			= compare x y

--
-- Changes the representation of a proposition already in DNF from data type Prop to type DNF.
-- Some remarks:
--  (*) all occurences of (Un Neg (Var x)) are changed to (Var ('~' : x))
--  (*) this function only works (correctly) if the input is already in DNF
--
propToDNF :: Prop -> DNF
propToDNF (Var x)		= [[x]]
propToDNF (Val b)		= [[lowerCaseBool b]]
propToDNF (Un Neg (Var p))	= [['~' : p]]
propToDNF (Bin Conj p q)	= [head (propToDNF p) ++ head (propToDNF q)]
propToDNF (Bin Disj p q)	= propToDNF p ++ propToDNF q

--
-- Takes a proposition from a random form to a Disjunctive Normal Form.
--
takeToDNF :: Prop -> Prop

takeToDNF (Bin Impl p q)		= takeToDNF (Bin Disj (Un Neg p) q)

takeToDNF (Bin Equiv p q)		= takeToDNF (Bin Disj (Bin Conj p q) (Bin Conj (Un Neg p) (Un Neg q)))

takeToDNF (Un Neg (Bin Conj p q))	= takeToDNF (Bin Disj (Un Neg p) (Un Neg q))
takeToDNF (Un Neg (Bin Disj p q))	= takeToDNF (Bin Conj (Un Neg p) (Un Neg q))
takeToDNF (Un Neg (Un Neg p))		= takeToDNF p
takeToDNF (Un Neg (Val True))		= Val False
takeToDNF (Un Neg (Val False))		= Val True
takeToDNF (Un Neg (Var x))		= Un Neg (Var x)
takeToDNF (Un Neg p)			= takeToDNF (Un Neg (takeToDNF p))

takeToDNF (Bin Conj p (Bin Disj q r))	= takeToDNF (Bin Disj (Bin Conj p q) (Bin Conj p r))
takeToDNF (Bin Conj (Bin Disj p q) r)	= takeToDNF (Bin Disj (Bin Conj p r) (Bin Conj q r))
takeToDNF (Bin Conj (Val True) p)	= takeToDNF p
takeToDNF (Bin Conj p (Val True))	= takeToDNF p
takeToDNF (Bin Conj (Val False) p)	= Val False
takeToDNF (Bin Conj p (Val False))	= Val False
-- After takeToDNF p we can get back four types of propositions:
--  (1) a disjunction
--  (2) a conjunction
--  (3) a single identifier or the negation of an identifier
--  (4) a single constant (true or false)
-- Other variants are not possible because the result is a DNF.
-- In cases (1) and (4) we need to recall takeToDNF on the result of the conjunction because there
-- are situations when the result of the conjunction of the two resulting DNF's might not be a DNF.
-- For (1), an example would be (p /\ q \/ r) /\ t, if the two resulting DNF's are p /\ q \/ r and
-- t, respectively. For (4), an example would be true /\ p, if the two resulting DNF's are true and
-- p, respectively.
-- In cases (2) and (3), the simple conjunction of the result is sure to yield a DNF, so we don't
-- need to recall takeToDNF on it.
takeToDNF (Bin Conj p q) 		= if case_1_4 then takeToDNF (Bin Conj dnfp dnfq) else Bin Conj dnfp dnfq
  where
  dnfp		= takeToDNF p
  dnfq		= takeToDNF q
  -- neither of the let construct, where construct, simple equality and if construct allows pattern
  -- matching of a type constructor, so the case construct was the only alternative here...
  case1p	= case dnfp of (Bin Disj _ _) -> True ; otherwise -> False
  case4p	= case dnfp of (Val _) -> True ; otherwise -> False
  case1q	= case dnfq of (Bin Disj _ _) -> True ; otherwise -> False
  case4q	= case dnfq of (Val _) -> True ; otherwise -> False
  case_1_4	= case1p || case4p || case1q || case4q

takeToDNF (Bin Disj (Val True) p)	= Val True
takeToDNF (Bin Disj p (Val True))	= Val True
takeToDNF (Bin Disj (Val False) p)	= takeToDNF p
takeToDNF (Bin Disj p (Val False))	= takeToDNF p
-- The same argumentation as above, except that here (as opposed to above) case (1) can no longer
-- produce a non-DNF result, so we only need to take case (4) into account.
takeToDNF (Bin Disj p q) 		= if case4 then takeToDNF (Bin Disj dnfp dnfq) else Bin Disj dnfp dnfq
  where
  dnfp		= takeToDNF p
  dnfq		= takeToDNF q
  case4p	= case dnfp of (Val _) -> True ; otherwise -> False
  case4q	= case dnfq of (Val _) -> True ; otherwise -> False
  case4		= case4p || case4q

takeToDNF x				= x

--
-- Decides whether or not the proposition is a tautology by transforming its negation
-- to DNF and checking if its negation is a contradiction.
--
tautologyAlt :: Prop -> Bool
tautologyAlt = contradictionDNF . toDNF . Un Neg

--
-- Decides whether or not the proposition represented into DNF is a tautology.
-- Note: the atoms in each clause of the representation have to be sorted already for this
-- function to work (correctly).
--
contradictionDNF :: DNF -> Bool
contradictionDNF = null . filter (not . contr)

--
-- Decides whether or not an Clause is a contradiction.
-- Note: given the ordering of the atoms in a Clause that is generated by the sortBy function (used
-- in the toDNF function), I know that if both ~var & var appear in an Clause, then ~var will always
-- appear _before_ var, and I implicitly use this information in this function.
--
contr :: Clause -> Bool
contr []		= False
contr ["false"]		= True
contr (('~':x):y:rest)
  | x == y		= True
  | otherwise		= contr (y:rest)
contr (x:rest)		= contr rest


--
-- Another version of contr, using foldl, but less efficient because the entire list has to be traversed,
-- even if it's clear that Clause is a contradiction from, say, the first two atoms.
--
{-
contr :: Clause -> Bool
contr p = cont p ""

cont :: Clause -> (String -> Bool)
cont = foldl f (\_ -> False)
  where
  f _ "false"	= (\_ -> True)
  f e ('~':x)	= if (e "") then e else (\y -> if (x == y) then True else False)
  f e x		= if (e x) then e else (\_ -> False)
-}
