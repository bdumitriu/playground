-- | Defines general high level ant code
-- combinators.
module HLAntCombinators where

import HLAntCode
import Ant
import Antcode
import Cell

import Data.Bits
import Prelude hiding(Left, Right, drop, break, flip, not)

-- | An 'Either'
data Choice a b = With a | Without b 
            deriving (Show,Eq)

infixl 8 .&
infixl 7 .| 
infix 6 .@
infix 6 .*
infix 5 .<
infix 4 .+
infixl 3 .:

-- | And operator
(.&), (.|) :: Cond -> Cond -> Cond
cnd1 .& cnd2 = CAnd cnd1 cnd2 

-- | Or operator
cnd1 .| cnd2 = COr cnd1 cnd2

-- | If Then
(.<) :: Cond -> HLI -> (HLI -> HLI)
c .< ti = CIfThenElse c ti

-- | Else
(.+) :: (HLI -> HLI) -> HLI -> HLI
(.+) = ($)

-- | Sequential composition
(.:) :: HLI -> HLI -> HLI
i1 .: i2 = CSeq i1 i2

-- | DoUntil
(.@) :: HLI -> Cond -> HLI
li .@ c = CDoUntil c li

-- | While
(.*) :: Cond -> HLI -> HLI
c .* li = CWhile c li

-- | Not function
not :: Cond -> Cond
not = CNot

-- | True constant
true, false, move, pickUp :: Cond
true = CTrue

-- | False constant
false = CFalse

-- | Try to move forward. Could move on success,
-- couldn't move on failure
move = CMove

-- | Try to pickup food. Could pickup on success, 
-- couldn't pickup on failure.
pickUp = CPickUp

-- | Sense on a given 'SenseDir' for a given 'Cond'
sense :: SenseDir -> Condition -> Cond
sense = CSense

-- | Flip a coin with the given probability.
flip :: Int -> Cond
flip = CFlip

-- | Drops the food.
drop :: HLI
drop = CDrop

-- | Turns left.
turnLeft :: HLI
turnLeft = CTurn Left

-- | Turns right.
turnRight :: HLI
turnRight = CTurn Right

-- | Labels the current step with a 'String'
label :: String -> HLI
label = Label

-- | Jumps to a defined label.
goto :: String -> HLI
goto = Goto

-- | Raw marking version.
markRaw :: Marker -> HLI
markRaw = CMark

-- | Raw unmarking version.
unmarkRaw :: Marker -> HLI
unmarkRaw = CUnmark

-- | Sense at the current position for the given 'Marker'
senseHereForMarker :: Marker -> Cond
senseHereForMarker x = sense Here (Marker x)

-- | Tests if the number is marked here. 
marked :: Int -> Cond
marked n = rec (int2taggedMarks n)
  where rec []              = true
        rec ((Without h):t) = not (senseHereForMarker h) .& (rec t)
        rec ((With h):t)    = senseHereForMarker h .& (rec t)

marked' n i1 i2 = rec (int2taggedMarks n)
  where rec []              = i1
        rec ((Without h):t) = not (senseHereForMarker h) .< (rec t) .+ i2
        rec ((With h):t)    = senseHereForMarker h .< (rec t) .+ i2
        
-- | Marks, in the current place, an integer
-- encoded with the six different markers. There are
-- 64 different markings.
-- Note that this will not check if something is marked, and
-- will just erase any marker that could be in the position.
mark :: Int -> HLI
mark n | n < 0 || n > 63 = error "mark takes an integer from 0 to 63"
       | otherwise = process (int2taggedMarks n)
  where
    process []              = nop
    process ((With n):t   ) = markRaw n   .: process t
    process ((Without n):t) = unmarkRaw n .: process t

int2taggedMarks :: Int -> [Choice Marker Marker]
int2taggedMarks n = map (\x -> if (testBit (n+1) x) then (With x) else (Without x)) [0..5]

int2marks :: Int -> [Marker]
int2marks = rec . int2taggedMarks
  where
    rec [] = []
    rec ((With x):t) = x : rec t
    rec ((Without _):t) = rec t

-- | Unmarks an integer in the current place, first checking if
-- it is actually marked.
unmark :: Int -> HLI
unmark n | n < 0 || n > 63 = error "unmark takes an integer from 0 to 63"
         | otherwise = marked n .< (process l) .+ nop
  where process [] = nop
        process ((Without h):t) = process t
        process ((With h):t)    = unmarkRaw h .: process t
        l = int2taggedMarks n

-- | Does nothing.
nop :: HLI
nop = CNop

-- | Breaks from the current loop.
break :: HLI
break = CBreak

-- | Transforms a list of 'HLI's into a single
-- one, sequencing them. Might be useful for
-- layout purposes.
unite :: [HLI] -> HLI
unite = foldr (.:) nop

-- | Repeats the given 'HLI' n times.
repeatN :: Int -> HLI -> HLI
repeatN n = unite
            . take n 
            . repeat