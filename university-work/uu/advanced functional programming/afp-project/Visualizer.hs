{-# OPTIONS -fglasgow-exts #-}
-- | The "Main" module defines the main 
-- part of the visualizer.
module Main where

import VisualizerLib

import System.Environment       (getArgs, withArgs)
import System.Console.GetOpt

import World hiding (get)
import VisualizerTools
import Graphics.UI.WX hiding (when)
import Graphics.UI.WXCore hiding (when)
import Control.Monad.State hiding (State, get)
import Prelude hiding (Left, Right, round)
import Data.Bits
import Data.Int

-- | The main function processes arguments and transfers control to the
-- 'mainloop' function
main :: IO ()
main = 
  do 
    -- Initializations
    args   <- getArgs >>= processArgs
    (b, a) <- readBoardFile (world args)
    rac    <- readAntFile (redAnt args)
    bac    <- maybe (return Nothing) (\x -> readAntFile x >>= return . Just) (blackAnt args)
    rds    <- return $ rounds args
    sd     <- return $ seed args
    o      <- return $ output args

    case bac of
      Nothing -> mainloop (newWorldState b a rac _uselessAnts sd o rds (Just hexSize))
      Just x  -> mainloop (newWorldState b a rac x sd o rds (Just hexSize))

    return ()

-- | Utility for GHCi users.
runMe = withArgs (words "-w worlds\\sample0.world -r ants\\solution-1.ant -b ants\\solution-2.ant") main
