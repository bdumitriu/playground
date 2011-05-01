import Parser
import Char
import Int

--
-- Exercise 4.1
--

data FloatLiteral = FL String String String String
		  deriving (Eq, Ord, Show)

dig :: Parser Char Char
dig = satisfy isDigit

digits :: Parser Char [Char]
digits = many1 dig

floatLiteral :: Parser Char FloatLiteral
floatLiteral = (\a _ b c d -> FL a b c d) <$> intPart <*> symbol '.' <*> option fractPart "" <*> option exponentPart "" <*> option ((\x -> [x]) <$> floatSuffix) ""
	<|> (\_ a b c -> FL "" a b c) <$> symbol '.' <*> fractPart <*> option exponentPart "" <*> option ((\x -> [x]) <$> floatSuffix) ""
	<|> (\a b c -> FL a "" b c) <$> intPart <*> exponentPart <*> option ((\x -> [x]) <$> floatSuffix) ""
	<|> (\a b c -> FL a "" b c) <$> intPart <*> option exponentPart "" <*> ((\x -> [x]) <$> floatSuffix)

intPart :: Parser Char [Char]
intPart = signedInteger

fractPart :: Parser Char [Char]
fractPart = digits

exponentPart :: Parser Char [Char]
exponentPart = (\_ a -> a) <$> exponentIndicator <*> signedInteger

signedInteger :: Parser Char [Char]
signedInteger = (++) <$> option sign "" <*> digits

exponentIndicator :: Parser Char Char
exponentIndicator = symbol 'e'
	<|> symbol 'E'

sign :: Parser Char [Char]
sign = const "-" <$> symbol '-'
	<|> const "" <$> symbol '+'

floatSuffix :: Parser Char Char
floatSuffix = symbol 'f'
	<|> symbol 'F'
	<|> symbol 'd'
	<|> symbol 'D'

--
-- Exercise 4.2
--

myread :: String -> Float
myread ""	= 0
myread s	= read s

evalFL :: String -> Float
evalFL fl = evalFL_ (fst (head (floatLiteral fl)))

evalFL_ :: FloatLiteral -> Float
evalFL_ (FL iPart fPart ePart _) = read (iPart ++ '.' : fPart) * 10**(myread ePart)
{-
  | (not . null $ ePart) && (head ePart == '-')	= read (iPart ++ '.' : fPart) / fromInteger (10^(myread . tail $ ePart))
  | otherwise					= read (iPart ++ '.' : fPart) * 10^(myread ePart)
-}