--module Chapter4 where

import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.List (foldl')

-----------------------
-- exercises page 84 --
-----------------------

-- 1
safeHead    :: [a] -> Maybe a
safeHead [] = Nothing
safeHead l  = Just (head l)

safeTail    :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail l  = Just (tail l)

safeLast    :: [a] -> Maybe a
safeLast [] = Nothing
safeLast l  = Just (last l)

safeInit    :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit l  = Just (init l)

-- 2
splitWith   :: (a -> Bool) -> [a] -> [[a]]
splitWith p = tail . _splitWith p

_splitWith      :: (a -> Bool) -> [a] -> [[a]]
_splitWith _ [] = [[]]
_splitWith p (x:xs)
  | p x        = build (_splitWith p xs)
  | otherwise  = let (y:ys) = _splitWith p xs in (x:y):ys
  where build l@([]:ys) = l
        build (y:ys)    = []:(y:ys)

-- command framework
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _               -> putStrLn "error: exactly two arguments needed"

        myFunction = transpose

-- 3
printFirstLetter = unlines . map (specialHead . words) . lines

specialHead [] = ""
specialHead l  = head l

-- 4
transpose = unlines . trans . lines

trans        :: [[Char]] -> [[Char]]
trans []     = []
trans (x:xs) = zipStrings x (trans xs)

zipStrings               :: [Char] -> [[Char]] -> [[Char]]
zipStrings "" r          = map (' ':) r
zipStrings l []          = map (:" ") l
zipStrings (l:ls) (r:rs) = (l:r):(zipStrings ls rs)

-----------------------
-- exercises page 97 --
-----------------------

-- 1, 2, 3
asInt_fold          :: String -> Int
asInt_fold ""       = 0
asInt_fold "-"      = 0
asInt_fold ('-':xs) = - asInt_fold xs
asInt_fold xs       = foldl' step 0 xs
  where step acc x = 10 * acc + digitToInt x

-- 4
type ErrorMessage = String

asInt_either          :: String -> Either ErrorMessage Int
asInt_either ""       = Right 0
asInt_either "-"      = Right 0
asInt_either ('-':xs) = case asInt_either xs of
  Right result -> Right (-result)
  err@(Left _) -> err
asInt_either xs       = foldl' step (Right 0) xs
  where step acc x = case acc of
          err@(Left _) -> err
          Right a      -> case errDigitToInt x of
             err@(Left _) -> err
             Right d      -> Right (10 * a + d)
        errDigitToInt x
          | x >= '0' && x <= '9' = Right (digitToInt x)
          | otherwise            = Left ("non-digit '" ++ [x] ++ "'")

-- 5, 6
concat1 :: [[a]] -> [a]
concat1 = foldr (++) []

-- 7a
takeWhile1      :: (a -> Bool) -> [a] -> [a]
takeWhile1 p [] = []
takeWhile1 p (x:xs)
  | p x         = x : (takeWhile1 p xs)
  | otherwise   = []

-- 7b
takeWhile2   :: (a -> Bool) -> [a] -> [a]
takeWhile2 p = foldr step []
  where step x acc
          | p x       = x:acc
          | otherwise = []

-- 8, 9 (clear, very inefficient)
groupBy1   :: (t -> t -> Bool) -> [t] -> [[t]]
groupBy1 p = foldl' step []
  where step [] z     = [[z]]
        step list z
          | p x z     = (init list) ++ [(x:xs) ++ [z]]
          | otherwise = list ++ [[z]]
          where (x:xs) = last list

-- 8, 9 (uglier, slightly more efficient)
groupBy2       :: (Eq t) => (t -> t -> Bool) -> [t] -> [[t]]
groupBy2 p xs
  | last == [] = []
  | otherwise  = all ++ [last]
  where (all, last)     = foldl' step ([], []) xs
        step ([], []) z = ([], [z])
        step (ys, (x:xs)) z
          | p x z       = (ys, (x:xs) ++ [z])
          | otherwise   = (ys ++ [(x:xs)], [z])

-- 10
any1 p = foldl' step False
  where step acc x
          | p x       = True
          | otherwise = acc

words1 text = case foldr step [""] text of
                "":xs -> xs
                xs    -> xs
  where step x l@(y:ys)
          | x == ' ' || x == '\t' || x == '\n' || x == '\r' = case y of
                                                                ""        -> l
                                                                otherwise -> ("":l)
          | otherwise                                       = ((x:y):ys)

unlines1 = foldr step ""
  where step x xs = x ++ "\n" ++ xs
