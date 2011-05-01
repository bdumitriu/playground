module Tune (parsetune) where

import Char
import Data.FiniteMap
import Debug.Trace

tune :: String
tune = trace ("Options: "++TUNEDATA) TUNEDATA

parsetune' :: String -> [(String,(Int,Int))]
parsetune' [] = [("",(0,1))]
parsetune' (c:cs) | isDigit c = if str=="" then 
                                 (str,(num+mul*(ord c-ord '0'),mul*10)):recs
                               else ("",(ord c-ord '0',10)):(str,(num,mul)):recs
                 | isAlpha c = (c:str,(num,mul)):recs
                 | otherwise = error ("invalid tune suffix "++(c:cs))
        where ((str,(num,mul)):recs) = parsetune' cs

parsetune'' :: String -> FiniteMap String Int
parsetune'' str = foldr add emptyFM (parsetune' str)
      where add (str',(num,_)) fm = addToFM fm str' num

parsetune :: FiniteMap String Int
parsetune = parsetune'' tune