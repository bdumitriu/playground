module Main (main) where

import RMonadTest

main = putStrLn . unlines . reverse $ getLog

getLog = case run 100 (factorial (-1) ||| (factorial 3)) of
           Just (r, log) -> log
           Nothing       -> ["unfinished"]
