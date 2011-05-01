{-# OPTIONS -fglasgow-exts #-}
module Main where

import SimulatorLib

import System.Environment       (getArgs)
import System.Console.GetOpt

import World
import Control.Monad.State hiding (State)
import Prelude hiding (Left, Right)

-- | The main function processes arguments and transfers control to the
-- 'mainloop' function
main :: IO ()
main = do args   <- getArgs >>= processArgs
          (b, a) <- readBoardFile (world args)
          rac    <- readAntFile (redAnt args)
          bac    <- maybe (return Nothing) (\x -> readAntFile x >>= return . Just) (blackAnt args)
          rds    <- return $ rounds args
          sd     <- return $ seed args
          o      <- return $ output args
          when o (do putStrLn ("random seed: " ++ show _SEED)
                     putStrLn ("\nAfter round 0...")
                     (showBoard b))
          res <- case bac of
            Nothing -> evalStateT mainloop (newWorldState b a rac _uselessAnts sd o rds Nothing)
            Just x  -> evalStateT mainloop (newWorldState b a rac x sd o rds Nothing)
          if o then return () else printResult res