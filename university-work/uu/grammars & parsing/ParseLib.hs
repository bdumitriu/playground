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
data Parentheses  =  Match Parentheses Parentheses
                  |  Empty
                  deriving Show

open   =  symbol '('
close  =  symbol ')'

parens  :: Parser Char Parentheses
parens  =  f  <$> open <*> parens <*> close <*> parens
       <|> succeed Empty
  where f a b c d = Match b d 

nesting  :: Parser Char Int
nesting  =  f <$> open <*> nesting <*> close <*> nesting
        <|> succeed 0
  where f a b c d = max (1+b) d

-- Type definition for expression parse tree

data Expr = Con Int
          | Var String
          | Fun String [Expr]
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr

-------------------------------------------------------------
-- Parser for expressions with two priorities

fact  :: Parser Char Expr
fact  =  Con <$> integer 
     <|> Var <$> identifier 
     <|> Fun <$> identifier <*> parenthesised (commaList expr)
     <|> parenthesised expr

term  :: Parser Char Expr
term  =  chainr fact
               (    const (:*:) <$> symbol '*'
                <|> const (:/:) <$> symbol '/'
               )

expr  :: Parser Char Expr
expr  =  chainr term
               (    const (:+:) <$> symbol '+' 
                <|> const (:-:) <$> symbol '-'
               )
-- Parser for expressions with aribitrary many priorities

type Op a  =  (Char,a -> a -> a)

fact'  :: Parser Char Expr
fact'  =  Con <$> integer 
      <|> Var <$> identifier 
      <|> Fun <$> identifier <*> parenthesised (commaList expr')
      <|> parenthesised expr'

gen        :: [Op a] -> Parser Char a -> Parser Char a
gen ops p  =  chainr p (choice (map f ops))
  where f (s,c) = const c <$> symbol s 

expr'  :: Parser Char Expr
expr'  =  foldr gen fact' [addis, multis]

multis  =  [ ('*',(:*:)), ('/',(:/:)) ]
addis   =  [ ('+',(:+:)), ('-',(:-:)) ]

-- Combinators for repetition

psequence         :: [Parser s a] -> Parser s [a]
psequence []      =  succeed []
psequence (p:ps)  =  list <$> p <*> psequence ps

psequence'  :: [Parser s a] -> Parser s [a]
psequence'  =  foldr f (succeed [])
  where  f p q = list <$> p <*> q

choice  :: [Parser s a] -> Parser s a
choice  =  foldr (<|>) failp
type TS2  =  ([(Station,Time,Time)],Station)

type Station  =  String
type Time     =  (Int,Int)

data Token  =  Station_Token String | Time_Token (Int,Int) deriving Show

-- Parsers from Chapter 4

{-
main = let results = filter 
                       (\(x,y) -> null y) 
                       (ts "Groningen 8:37 9:44 Zwolle 9:49 10:15 Utrecht 10:21 11:05 DenHaag")
       in if not (null results)
          then fst (head results)
          else error "TravellingSchemes: string cannot be parsed"
-}

station  :: Parser Char Station
station  =  identifier

time  :: Parser Char Time
time  =  (\x y z -> (x,z)) <$> natural <*> symbol ':' <*> natural 

departure, arrival :: Parser Char Time
departure  =  time
arrival    =  time

tsstring1  :: Parser Char Int
tsstring1  =  (\x y z -> sum x) <$> 
              many1((\u v w (xh,xm) y (zh,zm) -> (zh-xh)*60 + zm - xm) <$>   
                    spaces
                    <*> station 
                    <*> spaces
                    <*> departure 
                    <*> spaces 
                    <*> arrival                      
                   )
          <*> spaces
          <*> station

tsstring2  :: Parser Char Int
tsstring2  =  (\t u v w x y z -> sum w) <$> 
              station
              <*> spaces
              <*> departure
              <*> many((\s (vh,vm) w x y (zh,zm) u ->(zh-vh)*60 + zm-vm ) <$>   
                    spaces
                    <*> arrival
                    <*> spaces
                    <*> station 
                    <*> spaces
                    <*> departure 
                    <*> spaces
                   )
              <*> arrival                      
              <*> spaces
              <*> station
           <|> const 0 <$> station
  
spaces  :: Parser Char String
spaces  =  many (symbol ' ')

scanner :: String -> [Token]
scanner = map mkToken . words 

mkToken :: String -> Token
mkToken xs = if isDigit (head xs)
             then Time_Token (mkTime xs) 
             else Station_Token (mkStation xs)

parse_result     :: [(a,b)] -> a
parse_result xs  |  null xs = error "parse_result: could not parse the input"
                 |  otherwise = fst (head xs)

mkTime  :: String -> Time
mkTime  =  parse_result . time

mkStation  :: String -> Station
mkStation  =  parse_result . station

tstoken1  :: Parser Token TS2
tstoken1  =  (\x z -> (x,z)) <$> 
            many1((\u v w -> (u,v,w)) <$>   
                  tstation 
                  <*> tdeparture 
                  <*> tarrival                      
                 )
        <*> tstation

tstoken2  :: Parser Token Int
tstoken2  =  (\s t x y z -> sum x) <$> 
             tstation 
             <*> tdeparture 
             <*> many((\(uh,um) v (wh,wm) -> (wh-uh)*60+wm-um) <$>   
                      tarrival                      
                      <*> tstation 
                      <*> tdeparture 
                     )
             <*> tarrival
             <*> tstation
         <|> const 0 <$> tstation

tstation :: Parser Token Station
tstation (Station_Token s:xs) = [(s,xs)]
tstation _                    = []

tdeparture, tarrival :: Parser Token Time 
tdeparture (Time_Token (h,m):xs) = [((h,m),xs)]
tdeparture _                     = []

tarrival (Time_Token (h,m):xs) = [((h,m),xs)]
tarrival _                     = []
class  Parens a  where
   empty  ::  a
   match  ::  a -> a -> a

-- Parsers from Chapter 7

instance  Parens Parentheses  where
   empty  =  Empty
   match  =  Match

instance  Parens Int  where
   empty      =  0
   match b d  =  max (1+b) d 

parens'  :: Parens a => Parser Char a
parens'  =  (\a b c d -> match b d) <$> 
            open <*> parens' <*> close <*> parens'
        <|> succeed empty

-- Parsers from Chapter 8

data ExprAS  =  If ExprAS ExprAS ExprAS
             |  Apply ExprAS ExprAS
             |  ConInt Int
             |  ConBool Bool deriving Show

type ExprASAlgebra a = (a -> a -> a -> a
                       ,a -> a -> a
                       ,Int -> a
                       ,Bool -> a
                       )

foldExprAS :: ExprASAlgebra a -> ExprAS -> a
foldExprAS (iff,apply,conint,conbool) = fold
  where fold (If ce te ee) = iff (fold ce) (fold te) (fold ee)
        fold (Apply fe ae) = apply (fold fe) (fold ae)
        fold (ConInt i)    = conint i
        fold (ConBool b)   = conbool b
               

sp    :: Parser Char a -> Parser Char a
sp p  =  (\_ x -> x) <$> many (satisfy isSpace) <*> p

sptoken    :: [Char] -> Parser Char [Char]
sptoken t  =  sp (token t)

{-
sptoken :: String -> Parser Char String
sptoken s = (\a b c -> b) <$> 
            many (symbol ' ') <*> token s <*> many1 (symbol ' ')
-}

boolean = const True <$> token "True" <|> const False <$> token "False"

parseExpr  :: Parser Char ExprAS
parseExpr  =  expr0
  where expr0  =  (\a b c d e f -> If b d f) <$> 
                  sptoken "if" 
                  <*> parseExpr
                  <*> sptoken "then"
                  <*> parseExpr
                  <*> sptoken "else"
                  <*> parseExpr
              <|> expr1
        expr1  =  chainl expr2 (const Apply <$> many1 (symbol ' '))
              <|> expr2
        expr2  =  ConBool <$> boolean
              <|> ConInt <$> natural

data InstructionSM  =  LoadInt Int
                    |  LoadBool Bool
                    |  Call
                    |  SetLabel Label
                    |  BrFalse Label
                    |  BrAlways Label
 
type Label  =  Int
compile = foldExprAS compileAlgebra

compileAlgebra :: ExprASAlgebra (Label -> ([InstructionSM],Label))
compileAlgebra = (\cce cte cee -> \l -> let (cc,l') = cce (l+2)
                                            (tc,l'') = cte l'
                                            (ec,l''') = cee l''
                                in (   cc
                                    ++ [BrFalse l] 
                                    ++ tc
                                    ++ [BrAlways (l+1)] 
                                    ++ [SetLabel l]
                                    ++ ec 
                                    ++ [SetLabel (l+1)]
                                   ,l'''
                                   ) 
                 ,\cf cx -> \l -> let (xc,l')  = cx l 
                                      (fc,l'') = cf l'
                                  in (xc ++ fc ++ [Call],l'')
                 ,\i -> \l -> ([LoadInt i],l)
                 ,\b -> \l -> ([LoadBool b],l)
                 )

