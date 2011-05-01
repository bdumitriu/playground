import Parser
import Char

--
-- Recognizes a digit at the beginning of a string.
--
digit :: Parser Char Char
digit = satisfy isDigit

newDigit :: Parser Char Int
newDigit = f <$> digit
  where
  f ch = ord ch - ord '0'

data Parantheses = Match Parantheses Parantheses
		 | Empty
		 deriving (Show)

open = symbol '('
close = symbol ')'

parens :: Parser Char Parantheses
parens = (\x y z t -> Match y t) <$> open <*> parens <*> close <*> parens
	<|> succeed Empty

--
-- Exercise 3.5
--
ex35 = list <$> symbol 'a'

--
-- Exercise 3.6
--
-- for any parser p...
--ex36 = list <$> symbol 'a' <*> p

--
-- Exercise 3.7
--
booleans :: Parser Char Bool
booleans = const True <$> token "True"
	<|> const False <$> token "False"

--
-- Exercise 3.8
--
{-
dig :: Parser Char Char
dig = digit

digs :: Parser Char [Char]
digs = list <$> digit <*> digs
	<|> succeed ""

dig0 :: Parser Char Char
dig0 = satisfy isDigitPos
  where
  isDigitPos x = isDigit x && (read [x] :: Int) > 0

nat :: Parser Char [Char]
nat = list <$> dig0 <*> digs
	<|> token "0"

sign :: Parser Char Char
sign = symbol '-' <|> symbol '+'

z :: Parser Char [Char]
z = list <$> sign <*> nat
	<|> nat

uLetter :: Parser Char Char
uLetter = satisfy isLower

cLetter :: Parser Char Char
cLetter = satisfy isUpper

letter :: Parser Char Char
letter = uLetter <|> cLetter

identifier :: Parser Char [Char]
identifier = list <$> letter <*> sos

sos :: Parser Char [Char]
sos = list <$> letter <*> sos
	<|> list <$> dig <*> sos
	<|> succeed ""

dutchZipCode :: Parser Char [Char]
--dutchZipCode = list <$> dig0 <*> (list <$> dig <*> (list <$> dig <*> (list <$> dig <*> (list <$> cLetter <*> (:[]) <$> cLetter))))
dutchZipCode = f <$> dig0 <*> dig <*> dig <*> dig <*> cLetter <*> cLetter
  where f a b c d e f = a:b:c:d:e:[f]
-}

--
-- Exercise 3.9
--
data Pal2 = Nil | Leafa | Leafb | Twoa Pal2 | Twob Pal2
	  deriving (Eq, Ord, Show)

sa = symbol 'a'
sb = symbol 'b'

palin2 :: Parser Char Pal2
palin2 = succeed Nil
	<|> const Leafa <$> sa
	<|> const Leafb <$> sb
	<|> f <$> sa <*> palin2 <*> sa
	<|> g <$> sb <*> palin2 <*> sb
  where
  f _ p _ = Twoa p
  g _ p _ = Twob p

palina :: Parser Char Int
palina = succeed 0
	<|> const 1 <$> sa
	<|> const 1 <$> sb
	<|> f <$> sa <*> palina <*> sa
	<|> g <$> sb <*> palina <*> sb
  where
  f _ p _ = 2 + p
  g _ p _ = p

--
-- Exercise 3.10
--
data English = Sentence Subject Predicate
	     deriving (Eq, Ord, Show)

data Predicate = Pred1 Verb NounPhrase
	       | Pred2 AuxVerb Verb Noun
	       deriving (Eq, Ord, Show)

data NounPhrase = NP Adjective Noun
		deriving (Eq, Ord, Show)

type Subject = String
type Verb = String
type AuxVerb = String
type Adjective = String
type Noun = String

english :: Parser Char English
english = (\x y -> Sentence x y) <$> subject <*> predicate

subject :: Parser Char Subject
subject = token "they"

predicate :: Parser Char Predicate
predicate = (\x y -> Pred1 x y) <$> verb <*> nounPhrase
	<|> (\x y z -> Pred2 x y z) <$> auxVerb <*> verb <*> noun

verb :: Parser Char Verb
verb = token "are"
	<|> token "flying"

nounPhrase :: Parser Char NounPhrase
nounPhrase = (\x y -> NP x y) <$> adjective <*> noun

auxVerb :: Parser Char Verb
auxVerb = token "are"

noun :: Parser Char Verb
noun = token "planes"

adjective :: Parser Char Verb
adjective = token "flying"

--
-- Exercise 3.19
--
test :: Parser Char a -> String -> Bool
test p st = not . null . filter (null . snd) $ (p st)

--
-- Exercise 3.22
--

dig :: Parser Char Char
dig = digit

digs :: Parser Char [Char]
digs = many1 digit

dig0 :: Parser Char Char
dig0 = satisfy isDigitPos
  where
  isDigitPos x = isDigit x && (read [x] :: Int) > 0

nat :: Parser Char [Char]
nat = list <$> dig0 <*> many digit
	<|> token "0"

sign :: Parser Char String
sign = token "-" <|> token "+"

z :: Parser Char [Char]
z = (++) <$> option sign "" <*> nat

uLetter :: Parser Char Char
uLetter = satisfy isLower

cLetter :: Parser Char Char
cLetter = satisfy isUpper

letter :: Parser Char Char
letter = uLetter <|> cLetter

identifier :: Parser Char [Char]
identifier = list <$> letter <*> sos

sos :: Parser Char [Char]
sos = many (dig <|> letter)

--
-- Exercise 3.23
--

psequence :: [Parser s a] -> Parser s [a]
psequence [] 		= succeed []
psequence (p:ps)	= list <$> p <*> psequence ps

choice :: [Parser s a] -> Parser s a
choice []	= failp
choice (p:ps)	= p <|> choice ps

--
-- Exercise 3.24
--

token' :: Eq s => [s] -> Parser s [s]
token' = psequence . map symbol

--
-- Exercise 3.26
--

identifier' :: Parser Char String
identifier' = list <$> letter <*> greedy (choice [letter, digit, symbol '_'])

--
-- Arithmetic expressions.
--

--
-- Type definition for parse tree for arithmetic expressions.
--
data Expr = Con Int
	  | Var String
	  | Fun String [Expr]
	  | Expr :+: Expr
	  | Expr :-: Expr
	  | Expr :*: Expr
	  | Expr :/: Expr
	  deriving (Eq, Ord, Show)

--
-- Parser for expressions with two priorities.
--
fact :: Parser Char Expr
fact = Con <$> integer
	<|> Var <$> identifier
	<|> Fun <$> identifier <*> paranthesised (commaList expr)
	<|> paranthesised expr

integer :: Parser Char Int
integer = (const negate <$> (symbol '-')) `option` id <*> natural

natural :: Parser Char Int
natural = (\x -> read x) <$> nat

term :: Parser Char Expr
term = chainr fact
	(
		const (:*:) <$> symbol '*'
		<|> const (:/:) <$> symbol '/'
	)

expr :: Parser Char Expr
expr = chainr term
	(
		const (:+:) <$> symbol '+'
		<|> const (:-:) <$> symbol '-'
	)

--
-- Exercise 3.27
--

expr' :: Parser Char Expr
expr' = greedyr (term <|> termmin) (const (:+:) <$> symbol '+')

expr'' :: Parser Char Expr
expr'' = chainr (chainl term (const (:-:) <$> symbol '-')) (const (:+:) <$> symbol '+')

termmin :: Parser Char Expr
termmin = greedyl term (const (:-:) <$> symbol '-')

greedyr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
greedyr pe po = h <$> greedy (j <$> pe <*> po) <*> pe
  where
  j x op = (x `op`)
  h fs x = foldr ($) x fs

greedyl :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
greedyl pe po = h <$> pe <*> greedy (j <$> po <*> pe)
  where
  j op x = (`op` x)
  h x fs = foldl (flip ($)) x fs

--
-- Exercise 3.29
--

data Mir = MirEmpty | MirA Mir | MirB Mir
	 deriving (Eq, Ord, Show)

mir :: Parser Char Mir
mir = (\_ x _ -> MirA x) <$> symbol 'a' <*> mir <*> symbol 'a'
	<|> (\_ x _ -> MirB x) <$> symbol 'b' <*> mir <*> symbol 'b'
	<|> succeed MirEmpty

--
-- Exercise 3.30
--

data BitList = SingleB Bit | ConsB Bit BitList
	     deriving (Eq, Ord, Show)

data Bit = Bit0 | Bit1
	 deriving (Eq, Ord, Show)

bitList :: Parser Char BitList
bitList = (\x _ y -> ConsB x y) <$> bit <*> symbol ',' <*> bitList
	<|> SingleB <$> bit

bit :: Parser Char Bit
bit = const Bit0 <$> symbol '0'
	<|> const Bit1 <$> symbol '1'

--
-- Exercise 3.31
--

fixedPoint :: Parser Char [Char]
fixedPoint = (++) <$> z <*> option (list <$> symbol '.' <*> digs) ""

--
-- Exercise 3.32
--

floatingPoint :: Parser Char [Char]
floatingPoint = (++) <$> fixedPoint <*> option (list <$> symbol 'E' <*> z) ""

--
-- Exercise 3.33
--

data Asgn = Asgn String Expr
	  deriving (Eq, Ord, Show)

jAsgn :: Parser Char Asgn
jAsgn = (\x _ y -> Asgn x y) <$> identifier' <*> symbol '=' <*> expr
