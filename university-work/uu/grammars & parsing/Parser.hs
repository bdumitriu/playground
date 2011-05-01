module Parser (
	Parser,
	symbol,
	satisfy,
	token,
	failp,
	succeed,
	epsilon,
	(<*>),(<|>),(<$>),
	many,
	many1,
	option,
	pack,
	listOf,
	list,
	first,
	greedy,
	greedy1,
	paranthesised,
	bracketed,
	commaList,
	semicolonList,
	chainr,
	chainl
)

where

--
-- The type for parsers.
--
type Parser symbol result = [symbol] -> [(result, [symbol])]

{--- Elementary parsers ---}

--
-- Recognizes a single symbol.
--
symbol :: Eq s => s -> Parser s s
symbol _ []	= []
symbol s (x:xs)
  | s == x	= [(x, xs)]
  | otherwise	= []

--
-- Recognizes any one symbol satisfying a predicate.
--
satisfy :: Eq s => (s -> Bool) -> Parser s s
satisfy _ []	= []
satisfy p (x:xs)
  | p x		= [(x, xs)]
  | otherwise	= []

--
-- Recognizes a token (a sequence of symbols).
--
token :: Eq s => [s] -> Parser s [s]
token token xs
  | take n xs == token	= [(token, drop n xs)]
  | otherwise		= []
  where
  n = length token

--
-- Fails (miserably :) in all situations.
--
failp :: Parser s a
failp _ = []

--
-- Succeeds recognizing a desired symbol in any sequence without eating any symbols.
--
succeed :: a -> Parser s a
succeed a xs = [(a, xs)]

--
-- Succeeds recognizing the empty string in any sequence.
-- 
epsilon :: Parser s ()
epsilon xs = [((), xs)]

{--- Basic parser combinators ---}

infixl 6 <*>
infixr 4 <|>
infixl 7 <$>

--
-- Returns a parser which combines the output of the two parsers into a single list.
--
(<|>) :: Parser s a -> Parser s a -> Parser s a
(p <|> q) xs = p xs ++ q xs

--
-- Returns a parser which pipes the two parsers in the following manner:
--  (*) it uses the remaining string after the parse of the first parser as input for the second
--      parser
--  (*) it combines the results of the two parsers by means of function application (first
--      parser's result has to be a function which is applied to the second parser's result).
--
(<*>) :: Parser s (b -> a) -> Parser s b -> Parser s a
(p <*> q) xs = [(f x, zs) | (f, ys) <- p xs, (x, zs) <- q ys]

--
-- Returns a parser which parses the same string(s) as the original parsers, but which then 
-- applies a function to the output.
--
--
(<$>) :: (a -> b) -> Parser s a -> Parser s b
(f <$> p) xs = [(f x, ys) | (x, ys) <- p xs]

{--- EBNF parser combinators ---}

--
-- Just the : operator as a function.
--
list :: a -> [a] -> [a]
list a as = a:as

--
-- many P = P* from EBNF
--
many :: Parser s a -> Parser s [a]
many p = list <$> p <*> many p
	<|> succeed []

--
-- many1 P = P+ from EBNF
--
many1 :: Parser s a -> Parser s [a]
many1 p = list <$> p <*> many p

--
-- option P = P? from EBNF
--
option :: Parser s a -> a -> Parser s a
option p d = p
	<|> succeed d

--
-- Returns a parser which would behave the same as if the three separate parsers would be called one
-- after the other, but only returns the output of the second parser.
--
pack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack p q r = (\_ x _ -> x) <$> p <*> q <*> r

paranthesised p = pack (symbol '(') p (symbol ')')
bracketed p = pack (symbol '[') p (symbol ']')
compund p = pack (token "begin") p (token "end")

--
-- Returns a parser which will parse a list of symbols separated by a custom separator. The first parser
-- should be the parser for symbols and the second should be the one for separators. The result will be
-- the list of symbols, with all the separators discarded.
--
listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = list <$> p <*> many ((\_ x -> x) <$> s <*> p)

commaList :: Parser Char a -> Parser Char [a]
commaList p = listOf p (symbol ',')

semicolonList :: Parser Char a -> Parser Char [a]
semicolonList p = listOf p (symbol ';')

first :: Parser s b -> Parser s b
first p xs
  | null r	= []
  | otherwise	= [head r]
  where
  r = p xs

greedy :: Parser s b -> Parser s [b]
greedy = first . many

greedy1 :: Parser s b -> Parser s [b]
greedy1 = first . many1

--
-- Returns a parser for expressions like e1 (+) e2 (+) ... (+) en, with (+) being a generic operator, which
-- considers (+) to be a right associative operator. The first parser is for the expressions, the second one
-- is for the operators.
--
chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr pe po = h <$> many (j <$> pe <*> po) <*> pe
  where
  j x op = (x `op`)
  h fs x = foldr ($) x fs

--
-- Returns a parser for expressions like e1 (+) e2 (+) ... (+) en, with (+) being a generic operator, which
-- considers (+) to be a left associative operator. The first parser is for the expressions, the second one
-- is for the operators.
--
chainl :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl pe po = h <$> pe <*> many (j <$> po <*> pe)
  where
  j op x = (`op` x)
  h x fs = foldl (flip ($)) x fs