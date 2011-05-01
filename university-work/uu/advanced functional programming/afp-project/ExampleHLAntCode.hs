-- | Presents some examples of high level
-- ant code.
module ExampleHLAntCode where

import Ant
import Antcode
import HLAntCombinators
import HLAntCode
import Prelude hiding(Left, Right, drop, break, flip, not)

-- | Probabilistic 'wanderAround'. Will 
-- probably wander around n steps.
wanderAroundP :: Int -> HLI
wanderAroundP n = turnLeft .@ (move .& (flip n))

-- | Walk on straight line until collision
walkThen :: HLI
walkThen = nop .@ move

-- | Walk on straight line for n steps. If at anytime 
-- can't walk, jump to the given 'HLI'. Not efficient.
line :: Int -> HLI -> HLI
line 0 _ = nop
line n i = move .< (line (pred n) i) .+ i

-- | Non-deterministic, efficient cousin of 'line'.
lineP :: Int -> HLI
lineP n = nop .@ ((not $ flip n) .& move)

-- | Reverses the direction
turnBack :: HLI
turnBack = repeatN 3 turnLeft

-- copied from World.hs, to avoid importing WxHaskell as well
turn :: LeftOrRight -> Dir -> Dir
turn Left d  = (d+5) `mod` 6
turn Right d = (d+1) `mod` 6

-- | Inverts a given 'Dir'.
invert :: Dir -> Dir
invert d = (d+3) `mod` 6

-- | Checks for food in the 3 front cells
food3Around :: Cond
food3Around = (sense Ahead Food) .|
              (sense LeftAhead Food) .|
              (sense RightAhead Food)