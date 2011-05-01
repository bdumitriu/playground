{-# OPTIONS -fth #-}
-- The above should be OPTIONS_GHC, but let's keep it like
-- this to ensure backward compatibility
module Test where

import Data.Char (toUpper)
import Language.Haskell.THSyntax
import Correct
import GHC.Base

-- Answer: [2,4,6,8,10]
test0 = $(remove 2) 
        filter even () [1..10]
          
-- Answer: [1,2,3,4]
test1 = $(remove 0) 
        True takeWhile (<5) [1..10]

-- Answer: "AFP 2005*** Exception: inserted argument
test2 = $(insertHole 2)
        foldr ((:) . toUpper) "afp 2005"

-- Bonus
test3 = $(insert 2 [| [] |])
        foldr ((:) . toUpper) "afp 2005"

-- Answer: 55
test4 = $(permute [0,2,1])
        foldr 0 (+) [1..10]

-- Answer: 5
test5 = $(parens 1 3)
        length filter even [1..10]

-- Bonus
--test6 = $(unparens 1)
--        foldr ((++) [] ["Template", "Meta", "Programming"]) 

-- Bring into scope 11 insert functions:
$(mapM (\x -> insertDecl x [| [] |]) [0..10])