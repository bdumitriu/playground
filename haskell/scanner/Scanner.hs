module Scanner where

import Char

-- the data type Token with constructors for
-- each type of Token that has to be supported
data Token = Id Char                  -- identifiers
           | Num Int                  -- numbers
           | Add | Subt | Mul | Div   -- the four operators
           | LParen | RParen          -- left and right parenthesis
           deriving (Eq, Show)

-- the tokenize function takes a string and returns a
-- list of tokens or ends with an error message (in case
-- of a scan error)
tokenize :: String -> [Token]

-- the empty string generates an empty list of tokens
tokenize ""       = []

-- each of the characters '(', ')', '+', '-', '*', '/'
-- is translated directly into the corresponding token
tokenize ('(':xs) = LParen : (tokenize xs)
tokenize (')':xs) = RParen : (tokenize xs)
tokenize ('+':xs) = Add    : (tokenize xs)
tokenize ('-':xs) = Subt   : (tokenize xs)
tokenize ('*':xs) = Mul    : (tokenize xs)
tokenize ('/':xs) = Div    : (tokenize xs)

tokenize (x:xs)
  -- we skip over white space (i.e., we don't generate any token)
  | isSpace x     = tokenize xs

  -- a single alphabetic character is an identifier
  | isAlpha x     = Id x : (tokenize xs)

  -- if we see a digit, then we take that and all the subsequent
  -- digits and pass them to getNum in order to get back a Num
  -- token with the proper number as an Int; for continuing the
  -- scanning process, the tokenize function is not called on xs
  -- (as in all the other cases) but on whatever comes after the
  -- sequence of digits
  | isDigit x     = (getNum (takeWhile isDigit (x:xs))) : tokenize (dropWhile isDigit xs)

  -- encounring any symbol not recognized above results in a scanner
  -- error
  | otherwise     = error ("Scanner error: unrecognized character: " ++ [x])

-- takes a string that is already known to contain only digits and
-- returns a Num token to represent it. The string is converted to
-- an Int using the function read. We need to explicitly type the
-- result of read n to Int (by saying :: Int) so that the read function
-- knows what to translate the string to.
getNum :: String -> Token
getNum n = Num (read n :: Int)

