{-# OPTIONS -fglasgow-exts #-}
module SimulatorLib where

import System.Environment       (getArgs)
import System.Console.GetOpt

import World
import Control.Monad.State hiding (State)
import Prelude hiding (Left, Right)

data FlagType = World 
              | RedAnt
              | BlackAnt
              | Rounds
              | Seed
              | Output
               deriving Eq

type Flag = (FlagType, String)

data Args = Args { world    :: FilePath
                 , redAnt   :: FilePath
                 , blackAnt :: Maybe FilePath
                 , rounds   :: Int
                 , seed     :: Int
                 , output   :: Bool
                 }

newArgs :: FilePath -> FilePath -> Maybe FilePath -> Maybe String -> Maybe String -> Maybe String -> Args
newArgs wf rf bf rds sd opt =
  let opt' = maybe False (const True) opt
      sd'   = maybe 12345 read sd
      rds' = maybe 100000 read rds
  in Args { world    = wf
          , redAnt   = rf
          , blackAnt = bf
          , rounds   = rds'
          , seed     = sd'
          , output   = opt'
          }

{- Functions for reading command line options -}

-- | Takes a list of arguments (presumably obtained by calling 'getArgs') and
--   returns an 'Args' instance which contains the specified arguments or some
--   default values (for the unspecified arguments). It can also return an
--   error message if the options are incorrect or incomplete.
processArgs :: [String] -> IO Args
processArgs args = do (os, _, es) <- return (getOpt Permute commandLineOptions args)
                      case es of
                        []        -> return ()
                        _         -> wrapError (concat es)
                      wf <- maybe (wrapError "world file not specified\n") (return . id) . lookup World $ os
                      rf <- maybe (wrapError "red ant file not specified\n") (return . id) . lookup RedAnt $ os
                      let bf     = lookup BlackAnt os
                          rds = lookup Rounds os
                          sd   = lookup Seed os
                          opt = lookup Output os
                      return (newArgs wf rf bf rds sd opt)

commandLineOptions :: [OptDescr Flag]
commandLineOptions =
  [ Option ['w'] ["world"]       (ReqArg ((,) World) "FILE")    "the world file to use"
  , Option ['r'] ["red-ant"]     (ReqArg ((,) RedAnt) "FILE")   "the ant code file for red ants"
  , Option ['b'] ["black-ant"]   (ReqArg ((,) BlackAnt) "FILE") "the ant code file for black ants (optional)"
  , Option ['c'] ["nr-rounds"]   (ReqArg ((,) Rounds) "INT")    "the number of rounds to simulate (optional)"
  , Option ['s'] ["random-seed"] (ReqArg ((,) Seed) "INT")      "the initial seed for the RNG (optional)"
  , Option ['o'] ["output"]      (NoArg (Output, ""))           "use if you want to dump output (optional)"
  ]

wrapError :: String -> IO a
wrapError = ioError . userError . (++ usageMsg)

usageMsg :: String
usageMsg = usageInfo usageHeader commandLineOptions

usageHeader :: String
usageHeader = "Usage: <program-name> -w worldFile -r redAntFile [-b blackAntFile] [-c numberOfRounds]"
 ++ "[-s randomSeed] [-o]\n"

printResult :: (Int, Int) -> IO ()
printResult (redFood, blackFood) = do
          putStrLn $ "Red ants collected " ++ show redFood ++ " food."
          putStrLn $ "Black ants collected " ++ show blackFood ++ " food."
          case redFood `compare` blackFood of
            GT -> putStrLn "Red team win 2 points"
            LT -> putStrLn "Black team win 2 points"
            EQ -> putStrLn "It is a draw, each team wins 1 point."
          return ()

-- Return food at both anthills: (Red, Black)
mainloop :: World (Int, Int)
mainloop =
  do rds <- getRds
     o <- getO
     i <- getRound
     if (i == rds)
      then do redFood <- foodAtAnthill Red
              blackFood <- foodAtAnthill Black
              return (redFood, blackFood)
      else do a<- getAnts
              b <- getBoard
              x <- return $ indices a
              mapM_ step x
              when o
                (liftIO $ putStrLn ("\nAfter round " ++ show (succ i) ++ "...") >> (showBoard b))
              updateRound
              mainloop


showBoard :: Board -> IO ()
showBoard b = do cells <- getAssocs b
                 temp <- return (map (\(p, c) -> ("cell " ++ show p ++ ": " ++ (show c))) cells)
                 mapM_ putStrLn temp
