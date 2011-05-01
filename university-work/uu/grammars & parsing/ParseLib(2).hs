module ParseLib where

import Char

infixl 7  <$>
infixl 6  <*>
infixr 4  <|>

-- The type of parsers

type Parser symbol result  =  [symbol] -> [(result,[symbol])]

-- Elementary parsers

symbol  ::  Eq s  =>  s -> Parser s s
symbol a []                  =  []
symbol a (x:xs) | x == a     =  [(x,xs)]
                | otherwise  =  []

satisfy  ::  (s -> Bool) -> Parser s s
satisfy p []                 =  []
satisfy p (x:xs) | p x       =  [(x,xs)]
                 | otherwise =  []

token  ::  Eq s => [s] -> Parser s [s]
token k xs  |  k == take n xs  =  [(k,drop n xs)]
            |  otherwise       =  []
  where  n = length k

failp     :: Parser s a
failp xs  =  []

succeed       :: a -> Parser s a
succeed r xs  =  [(r,xs)]

-- Applications of elementary parsers

digit  :: Parser Char Char
digit  =  satisfy isDigit

-- Parser combinators 

(<|>)         :: Parser s a      -> Parser s a -> Parser s a
(p <|> q) xs  =  p xs ++ q xs

(<*>)         :: Parser s (b -> a) -> Parser s b -> Parser s a
(p <*> q) xs  =  [(f x,zs)
                 |(f  ,ys) <- p xs
                 ,(  x,zs) <- q ys
                 ]

(<$>)         :: (a -> b) -> Parser s a -> Parser s b
(f <$> p) xs  =  [(f y,ys)
                 |(  y,ys) <- p xs
                 ]

-- Applications of parser combinators

newdigit  :: Parser Char Int
newdigit  =  f <$> digit
  where  f c = ord c - ord '0'

-- EBNF parser combinators 

option      :: Parser s a -> a -> Parser s a
option p d  =  p <|> succeed d

many    :: Parser s a  -> Parser s [a]
many p  =  list <$> p <*> many p <|> succeed []

many1    :: Parser s a -> Parser s [a]
many1 p  =  list <$> p <*> many p

pack        :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack p r q  =  pi32 <$> p <*> r <*> q 

listOf      :: Parser s a -> Parser s b -> Parser s [a]
listOf p s  =  list <$> p <*> many (pi22 <$> s <*> p)

-- Auxiliary functions

determ  ::  Parser s b -> Parser s b
determ p xs  |  null r     =  []
             |  otherwise  =  [head r]
  where r = p xs

greedy, greedy1  ::  Parser s b -> Parser s [b]
greedy   =  determ . many
greedy1  =  determ . many1

list x xs  =  x:xs

pi22 x y    =  y
pi32 x y z  =  y

-- Applications of EBNF combinators

natural  :: Parser Char Int
natural  =  foldl (\a b -> a*10 + b) 0 <$> many1 newdigit

integer  ::  Parser Char Int
integer  =  (const negate <$> (symbol '-')) `option` id  <*>  natural 

identifier :: Parser Char String
identifier =  list <$> satisfy isAlpha <*> greedy (satisfy isAlphaNum)

parenthesised p  =  pack (symbol '(') p (symbol ')')

commaList    :: Parser Char a -> Parser Char [a]
commaList p  =  listOf p (symbol ',')

-- Chain expression combinators

chainr  ::  Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr pe po  =  h <$> many (j <$> pe <*> po) <*> pe
  where j x op  =  (x `op`)
        h fs x  =  foldr ($) x fs

chainl  ::  Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl pe po  =  h <$> pe <*> many (j <$> po <*> pe)
  where j op x  =  (`op` x)
        h x fs  =  foldl (flip ($)) x fs


-- Combinators for repetition (exercise 3.23)

psequence         :: [Parser s a] -> Parser s [a]
psequence []      =  succeed []
psequence (p:ps)  =  list <$> p <*> psequence ps

psequence'  :: [Parser s a] -> Parser s [a]
psequence'  =  foldr f (succeed [])
  where  f p q = list <$> p <*> q

choice  :: [Parser s a] -> Parser s a
choice  =  foldr (<|>) failp
