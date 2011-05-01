{-
	S -> eps | BS
	B -> 0 | 1

                 S
             /       \
            B         S
            |      /     \
            1     B       S
                  |     /   \
                  0    B     S
                       |    / \ 
                       1   B   S
                           |   |
                           1  eps

              (11,4)
             /      \
            1      (3,3)   
            |     /     \
            1    0     (3,2) 
                 |     /   \
                 0    1   (1,1) 
                      |    / \ 
                      1   1 (0,0)
                          |   |
                          1  eps


-}

{- P:/ipt/uuag -a STAttributes.ag -}

module Chapter3 where

import UU.Parsing

instance Symbol Char

test :: Show a => Parser Char a -> String -> IO ()
test p input =
	do	result <- parseIO p input
		putStrLn (show result)

data Bin = Bin_Empty | Bin_Zero Bin | Bin_One Bin
	 deriving Show

pBitString :: Parser Char Bin
pBitString = ($) <$> pBit <*> pBitString
	<|> Bin_Empty <$ pSucceed ""

pBit :: Parser Char (Bin -> Bin)
pBit = const Bin_Zero <$> pSym '0'
	<|> const Bin_One <$> pSym '1'

pBitString' :: Parser Char (Int, Int)
pBitString' = (\b (r, p) -> (r + b * p, p * 2)) <$> pBit' <*> pBitString'
	<|> (0, 1) <$ pSucceed ""

pBit' :: Parser Char Int
pBit' = const 0 <$> pSym '0'
	<|> const 1 <$> pSym '1'


