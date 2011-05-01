-- | The "Cell" module defines the 'Cell' data structure and a constructor for
--   instances of this data structure.
module Cell (
  module Ant, module Cell
  ) where

import Ant
import Data.Bits
import Data.Int
import Control.Monad.State

-- | The 'Marker' can be an 'Int' between 0 and 5.
type Marker = Int

-- | The data type 'Cell' stores the elements which define a 'Cell'.
data Cell = Cell { isRocky   :: Bool            -- ^ 'True' if Cell is rocky, 'False' if it is clear

                 , ant       :: Maybe Ant       -- ^ either one or no 'Ant' can be in a 'Cell'

                 , anthill   :: Maybe Color     -- ^ either a 'Color', if 'Cell' is an anthill, or
                                                --   'Nothing' if it is not

                 , food      :: Int             -- ^ (non-negative) amount of food in 'Cell'

                 , raMarkers :: Int8            -- ^ one bit for each of the six 'Marker's of the
                                                --   'Red' 'Ant's, two bits unused

                 , baMarkers :: Int8            -- ^ one bit for each of the six 'Marker's of the
                                                --   'Black' 'Ant's, two bits unused
                 }

instance Show Cell where
  show (Cell r a ah f rm bm) =
    (if r 
      then "rock" 
      else (if (f > 0) then (show f ++ " food; ") else "")
           ++
           (case ah of
        Nothing    -> ""
        Just Red   -> "red hill; "
        Just Black -> "black hill; "))
    ++ printMarkers rm' "red marks: "
    ++ printMarkers bm' "black marks: "
    ++ (case a of
          Just x -> show x
          Nothing  -> "") 
      where rm' = mkMarkers rm
            bm' = mkMarkers bm
            printMarkers m s = if (null m) then "" else (s ++ m ++ "; ")
            mkMarkers m = concatMap (\x -> if (testBit m x) then (show x) else "") [0..5]

hasAnt :: Cell -> Bool
hasAnt c = maybe False (const True) (ant c)

-- | Takes a list of 'Char's which encode 'Cell' types and returns the list of
--   generated 'Cell's inside a 'State' monad. The state of this monad encodes
--   the id to give to an 'Ant', if the 'Cell' contains one. Thus, the 'State'
--   monad should be run with the value to be given to the first 'Ant'.
--
--   See 'newCell' for 'Char' encodings of 'Cell's.
newCells :: [Char] -> State Int [Cell]
newCells = mapM newCell

-- | Generates a 'Cell' based on a 'Char'. The 'Char' encodings for 'Cell's are
--   as follows:
--
--    * \'#\' rocky 'Cell'
--
--    * \'.\' clear 'Cell' (containing nothing interesting)
--
--    * \'+\' 'Red' anthill 'Cell'
--
--    * \'-\' 'Black' anthill 'Cell'
--
--    * \'1\' to \'9\' clear 'Cell' containing the given number of food particles
newCell :: Char -> State Int Cell
newCell c = case c of
    '#'         -> newRockyCell
    '.'         -> newClearCell
    '+'         -> newAnthillCell Red
    '-'         -> newAnthillCell Black
    _           -> newFoodCell (read [c])

newRockyCell :: State Int Cell
newRockyCell = return $ Cell True Nothing Nothing 0 0 0

newClearCell :: State Int Cell
newClearCell = return $ Cell False Nothing Nothing 0 0 0

newAnthillCell :: Color -> State Int Cell
newAnthillCell c = do i <- get
                      modify succ
                      return (Cell False (Just (newAnt i c)) (Just c) 0 0 0)

newFoodCell :: Int -> State Int Cell
newFoodCell i = return $ Cell False Nothing Nothing i 0 0
