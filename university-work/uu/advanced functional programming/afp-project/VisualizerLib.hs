{-# OPTIONS -fglasgow-exts #-}
-- | The "Main" module defines the main 
-- part of the visualizer.
module VisualizerLib where

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

-- | The size of the hexagon's side
hexSize :: Int
hexSize = 7

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

-- | Repeats a monadic computation n times
mRepeatN :: (Monad m) => Int -> m a -> m [a]
mRepeatN n = sequence . replicate n

-- | Repeats a monadic computation n times, returning the result
-- of the last computation
mRepeatN_ :: (Monad m) => Int -> m a -> m a
mRepeatN_ n = foldr1 (>>) . replicate n

-- | Transforms a pair of integers representing the amount of gathered
-- food by each team and returns a descriptive 'String' of the scoring results.
printResult :: (Int, Int) -> String
printResult (redFood, blackFood) =
  "Red ants collected " ++ show redFood ++ " food.\n" ++
   "Black ants collected " ++ show blackFood ++ " food.\n" ++
    case redFood `compare` blackFood of
      GT -> "Red team win 2 points\n"
      LT -> "Black team win 2 points\n"
      EQ -> "It is a draw, each team wins 1 point.\n"

-- | Here the GUI (wxHaskell based) is presented. Takes an initial 'WorldState'
-- as argument.
mainloop :: WorldState -> IO ()
mainloop s = start $ do
  currState <- varCreate s

  temp <- bitmapCreateDefault
  back <- varCreate temp
  ac <- varCreate False
  
  f <- frame [text := "AFP Project - Ant Visualizer - University Utrecht, 2005", clientSize := sz 300 200]
  frameSetIconFromFile f "images/ant.ico"
  
  board <- scrolledWindow f [on paint := cellPainter currState back ac]
  infoLine <- staticText f []
  bPanel <- panel f []
  mPanel <- panel bPanel [] -- the marker panel
  sPanel <- panel bPanel []

  step <- button bPanel [text := "St&ep"]
  start <- button bPanel [text := "&Start"]
  stop <- button bPanel [text := "&Stop", enabled := False]
  jump <- button bPanel [text := "&Jump to turn"]
  jump10 <- button bPanel [text := "Jump 10"]
  jump100 <- button bPanel [text := "Jump 100"]
  jump1000 <- button bPanel [text := "Jump 1000"]
  jumpEnd <- button bPanel [text := "Jump to end"]
  speedPlus <- smallButton sPanel [text := "+"]
  speedMinus <- smallButton sPanel [text := "-"]
    
  redM <- mapM (checkBox mPanel) (take 6 . repeat $ [{-checkable := False-}])
  blackM <- mapM (checkBox mPanel) (take 6 . repeat $ [{-checkable := False-}])

  currState' <- varGet currState
  turnNr <- textEntry bPanel [text := show $ round currState', Graphics.UI.WX.size := sz 70 22]
  refresh <- button bPanel [text := "&Refresh"]
  
  let buttons = [step, start, stop, jump, jump10, jump100, jump1000, 
                 jumpEnd, refresh, speedPlus, speedMinus]

  -- If the bool is True, then repaints also the board, if not
  -- then only updates the turnNr
  let 
    repaint' b  = do currState' <- varGet currState
                     when b $ repaint board
                     set turnNr [text := show $ round currState']

  t <- timer f [ interval := 50
               -- change the False below to True if the board should be automatically drawn at every 
               -- step (slow)
               , on command := roundHandler currState 1 f >> repaint' True
               , enabled := False ]
  let 
    updateTimer f = set t [interval :~ (\x -> if (x `f` 2 <= 0) then 1 else (x `f` 2))]
  
  set speedPlus [on command := updateTimer (div)]
  set speedMinus [on command := updateTimer (*)]

  set step [on command := roundHandler currState 1 f >> repaint' True]
  set start [on command := enable t >> disable step >> disable start >> enable stop >> focusOn stop]
  set stop [on command := disable t >> enable step >> disable stop >> enable start >> focusOn start]

  set refresh [on command := repaint' True]
  
  set jump [on command := do currState' <- varGet currState
                             newRound_ <- get turnNr text
                             let newRound' = reads newRound_
                             let newRound = case newRound' of
                                             [] -> -1
                                             ((x :: Int,_):_) -> x
                             let diff = newRound - (round currState')
                             set infoLine [text := ("Running " ++ (show diff) ++ " turns, please wait...")]
                             when (diff > 0) $ roundHandler currState diff f
                             set infoLine [text := "Done"]
                             repaint' True
           ]
  set jump10 [on command := do roundHandler currState 10 f 
                               repaint' True]
  set jump100 [on command := do mapM_ disable buttons 
                                roundHandler currState 100 f 
                                repaint' True
                                mapM_ enable buttons]
  set jump1000 [on command := do mapM_ disable buttons 
                                 roundHandler currState 1000 f 
                                 repaint' True
                                 mapM_ enable buttons]
  set jumpEnd [on command := do mapM_ disable buttons 
                                currState' <- varGet currState
                                let diff = 100001 - (round currState')
                                when (diff > 0) $ roundHandler currState diff f
                                repaint' True
                                mapM_ enable buttons]
  
  set refresh [on command := repaint' True]
  
  set sPanel [layout := row 5 [static $ widget speedPlus, static $ widget speedMinus]]
  
  set mPanel [layout := grid 5 1 ( [label "RM", label "BM", label "Nr"] :
                                   zipWith (:) (map widget redM)
                                               (zipWith (:) (map widget blackM)
                                                            (map ((:[]) . label . show) [0 .. 5])))
             ]
  
  set bPanel [layout := column 5 [static $ widget step
                                 ,static $ widget start
                                 ,static $ widget stop
                                 ,static $ widget refresh
                                 ,static $ widget jump10
                                 ,static $ widget jump100
                                 ,static $ widget jump1000
                                 ,static $ widget jump
                                 ,static $ widget turnNr
                                 ,static $ widget jumpEnd
                                 ,static $ widget sPanel
                                 ,static $ widget mPanel]
            ]


  set f [layout := margin 5 $ grid 5 5 [[vfill $ widget bPanel, fill $ minsize (sz 1 1) $ widget board]
                                       ,[empty, hfill $ widget infoLine]]
        ,clientSize := sz 800 600
        ]

  set board [virtualSize := sz 1600 1200, scrollRate := sz 10 10]
  set board [on motion := mouseHandler currState infoLine redM blackM]

  focusOn start

-- | Enables a widget
enable  b = set b [ enabled := True ]

-- | Disables a widget
disable b = set b [ enabled := False ]

-- | Paints all the board. This will call 'paintCells' and 'paintCells2',
-- although the first is optimized using a 'dcBufferWithRef' to draw it to a bitmap, 
-- and then that bitmap is used as background (avoiding to recalculate all the 
-- positions again).
--cellPainter :: Var WorldState -> DC a -> Rect -> IO ()
cellPainter ws back ac dc w = do 
  s <- varGet ws
  done <- varGet ac
  if (not done)
   then do
    ac `varSet` True
    dcBufferWithRef dc (Just back) (Rect 0 0 1600 1200) (\dc -> paintCells dc s hexSize)
   else do
    back' <- varGet back
    dcDrawBitmap dc back' (Point 0 0) False
  
  paintCells2 dc s hexSize

-- | Handles a mouse event over a cell, displaying information about 
-- that cell.
mouseHandler :: Textual w => Var WorldState -> w -> [CheckBox ()] -> [CheckBox ()] -> Point -> IO ()
mouseHandler ws infoLine redM blackM p = do
  s <- varGet ws
  b <- return (board s)
  codes <- return (codes s)
  mc <- pointToCell p hexSize b
  case mc of
    Nothing       -> return () -- set infoLine [text := ""]
    Just (c, np)  -> do { set infoLine [text := (outputCellInfo c np codes)]
                        ; updateMarkers (raMarkers c) redM
                        ; updateMarkers (baMarkers c) blackM
                        }

updateMarkers :: Int8 -> [CheckBox ()] -> IO ()
updateMarkers m ms = mapM_ (\(n, cb) -> set cb [checked := testBit m n]) (zip [0 .. 5] ms)

-- | Runs n rounds, from an initial 'WorldState', and may present
-- a dialog to the parent window if the game is over.
roundHandler :: Var WorldState -> Int -> Window a -> IO ()
roundHandler ws n parent = 
  do s <- varGet ws
     (str,s') <- runStateT (mRepeatN_ n runOne) s
     ws `varSet` s'
     when (str /= "") $ do
       infoDialog parent "Results" str

-- | Runs a single round.
runOne :: World String
runOne = 
  do i <- getRound
     rds <- getRds
     if (i == rds)
      then do redFood <- foodAtAnthill Red
              blackFood <- foodAtAnthill Black
              updateRound
              return $ printResult (redFood, blackFood)
      else do 
              a <- getAnts
              b <- getBoard
              x <- return $ indices a
              mapM_ step x
              updateRound
              return ""
