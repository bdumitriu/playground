{-# OPTIONS -fglasgow-exts #-}
module VisualizerState where

import Interface
import Marks
import Data.FiniteMap
import Data.List
import Data.Maybe

(+.) :: Pos -> Pos -> Pos
(Pos x1 y1) +. (Pos x2 y2) = Pos (x1+x2) (y1+y2)

(*.) :: Int -> Pos -> Pos
s *. (Pos x2 y2) = Pos (s*x2) (s*y2)

(+..) :: Num a => (a,a) -> (a,a) -> (a,a)
(x1,y1) +.. (x2,y2) = (x1+x2,y1+y2)

(*..) :: Num a => a -> (a,a) -> (a,a)
s *.. (x2,y2) = (s*x2,s*y2)

data State = State { boardsize    :: !Int
                   , step     :: !Int
                   , rocks    :: !(FiniteMap Pos Rock)
                   , ants     :: !(FiniteMap Pos Ant)
                   , anthills :: !(FiniteMap Pos Anthill)
                   , food     :: !(FiniteMap Pos Food)
                   , marks    :: !(FiniteMap Pos Mark)
		   , comments :: !(FiniteMap AntID String)
		   , showblackmarks :: Bool
		   , showredmarks :: Bool
		   , showblackants :: Bool
		   , showredants :: Bool
		   , mousepos :: Pos
                   }
  deriving (Read,Show)

instance (Show a,Show b) => Show (FiniteMap a b) where
  show = show . fmToList

instance (Ord a,Read a,Read b) => Read (FiniteMap a b) where
  readsPrec i fm = let fml = (readsPrec i fm :: [([(a,b)],String)])
                   in  map (\(x,r) -> (listToFM x,r)) fml

type Ant = (HasFood,Colour,Dir,Resting,AntID)
type Anthill = Colour
type Rock = ()
rock = ()
type Food = Amount
type Mark = [Bool]
nomarks = replicate 12 False

setMark, unsetMark :: Colour -> Mark -> Int -> Mark
setMark c m n   = let (x,y:ys) = splitAt (n + colorOffset c) m
                  in x ++ (True : ys)

unsetMark c m n = let (x,y:ys) = splitAt (n + colorOffset c) m
                  in x ++ (False : ys)

colorOffset :: Colour -> Int
colorOffset Red   = 0
colorOffset Black = 6

marks2dir :: Mark -> Maybe Dir
marks2dir [b1,b2,b3] = case boolMarkToDir (b1,b2,b3) of
                         Dir d -> Just d
                         _     -> Nothing

getHomeDir :: Colour -> Mark -> Maybe Dir
getHomeDir c m = marks2dir $ take 3 (drop (colorOffset c) m)

getFoodDir :: Colour -> Mark -> Maybe Dir
getFoodDir c m = marks2dir $ take 3 (drop (colorOffset c + 3) m)


-- onn/on detects if a position is on the board
-- offn/off is the negation

onn,offn :: Int -> Pos -> Bool
on,off :: State -> Pos -> Bool

onn n (Pos x y) = x >= 0 && y >= 0 && x < n && y < n
offn n = not . onn n

on  s p = onn  (boardsize s) p
off s p = offn (boardsize s) p

emptyBoard = State { boardsize  = 100
                   , step = 0
                   , rocks = emptyFM
                   , ants  = emptyFM
                   , anthills = emptyFM
                   , food = emptyFM
                   , marks = emptyFM
		   , comments = emptyFM
		   , showblackmarks = True
		   , showblackants = True
		   , showredmarks = True
		   , showredants = True
                   , mousepos = Pos 0 0
                   }

-- processing commands:

procCmd :: VCommand -> State -> State
procCmd (VStep n) = \s -> s { step = n }
procCmd (VAnthill c pos) = \s -> s { anthills = addToFM (anthills s) pos c }
procCmd (VRock pos) = \s -> s { rocks = addToFM (rocks s) pos rock }
procCmd (VAnt hf c pos dir rest antid) = 
   \s -> s { ants = addToFM (ants s) pos (hf,c,dir,rest,antid) }
procCmd (VNoAnt pos) =
   \s -> s { ants = delFromFM (ants s) pos }
procCmd (VFood pos amount) = 
   \s -> s { food = addToFM (food s) pos amount }
procCmd (VMark c pos markid) =
   \s -> s { marks = let old = lookupWithDefaultFM (marks s) 
                                                   (nomarks)
                                                   pos
                     in  addToFM (marks s) pos (setMark c old markid)
           }
procCmd (VUnMark c pos markid) =
   \s -> s { marks = let old = lookupWithDefaultFM (marks s) 
                                                   (nomarks)
                                                   pos
                     in  addToFM (marks s) pos (unsetMark c old markid)
           }
procCmd (VComment antid _ comment) 
    = \s -> s { comments = addToFM (comments s) antid comment }

procCmd _ = id

getPos :: VCommand -> Maybe Pos
getPos (VAnthill c pos)          = Just pos
getPos (VRock pos)               = Just pos
getPos (VAnt hf c pos dir rest antid) = Just pos
getPos (VNoAnt pos)              = Just pos
getPos (VFood pos amount)        = Just pos
getPos (VMark c pos markid)      = Just pos
getPos (VUnMark c pos markid)    = Just pos
getPos _ = Nothing

