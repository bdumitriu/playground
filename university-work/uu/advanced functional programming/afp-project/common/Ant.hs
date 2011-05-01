-- | The "Ant" module defines the 'Ant' data structure as well as a few
--   ant-specific functions.
--
--   For detailed comments about most of the functions in this module, you are
--   referred to this document: <http://www.cis.upenn.edu/proj/plclub/contest/ants.html>.
module Ant (
  module Ant, get, put, liftIO
  ) where

import Control.Monad.State

-- | The color of the 'Ant's.
data Color = Red
           | Black
           deriving Eq

-- | Possible directions in which the 'Ant' can turn.
data LeftOrRight = Left | Right
                 deriving (Eq, Show)

-- | Possible positions which the 'Ant' can sense.
data SenseDir = Here
              | Ahead
              | LeftAhead
              | RightAhead
              deriving (Eq, Show)

-- | A direction is an 'Int' between 0..5.
type Dir = Int

-- | The 'Ant' state is simply an 'Int'.
type AntState = Int

-- | An 'Ant' is represented by an unique integer.
type AntId = Int

-- | The data type 'Ant' contains all information about a single 'Ant'.
data Ant = Ant { antid     :: AntId     -- ^ an identifier for the 'Ant'
               , color     :: Color     -- ^ the color of the 'Ant'
               , state     :: AntState  -- ^ the current state of the 'Ant''s brain
               , resting   :: Int       -- ^ how long the 'Ant' has to rest before doing any other action
               , direction :: Dir       -- ^ the 'Dir'ection in which the ant would move
               , hasFood   :: Bool      -- ^ whether the 'Ant' is currently carrying a food particle or not
               }

instance Show Color where
  show Red   = "red"
  show Black = "black"

instance Show Ant where
  show (Ant ai c s r d hf) =
    (show c) ++ " ant of id " ++ show ai ++ ", dir " ++ show d ++
    ", food " ++ (if hf then "1" else "0") ++ ", state " ++ 
    show s ++ ", resting " ++ show r

-- | Creates a new ant instance with the specified 'AntId' and 'Color'.
--   For the other fields, reasonable (expected) default values are used.
newAnt :: AntId -> Color -> Ant
newAnt i c = Ant { antid = i
                 , color = c
                 , state = 0
                 , resting = 0
                 , direction = 0
                 , hasFood = False
                 }

{- Biology functions -}

otherColor :: Color -> Color
otherColor Red   = Black
otherColor Black = Red

setState :: Ant -> AntState -> Ant
setState a s = a { state = s }

setResting :: Ant -> Int -> Ant
setResting a r = a { resting = r }

setDirection :: Ant -> Dir -> Ant
setDirection a d = a {direction = d}

setHasFood :: Ant -> Bool -> Ant
setHasFood a b = a {hasFood = b}
