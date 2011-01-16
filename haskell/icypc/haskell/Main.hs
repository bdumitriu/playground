module Main (main) where

import System.IO

import GameStateParser

readGameState 0 = return ""
readGameState n = do x <- getLine
                     rest <- readGameState (n - 1)
                     return (x ++ "\n" ++ rest)

loop h = do gameState <- readGameState 41
--            hPutStrLn h gameState
            hPutStrLn h (show . parseGameState $ gameState)
            hPutStrLn h (take 80 (repeat '='))
            putStrLn "run 0 3"
            putStrLn "crouch"
            putStrLn "crouch"
            putStrLn "crouch"
            hFlush stdout
            isEOF <- hIsEOF stdin
            if isEOF then return () else loop h

main = withFile "trace.trace" WriteMode loop
