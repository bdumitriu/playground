module Walk (walkrandomly) where

import Prelude hiding (Left,Right)
import Library

import Data.FiniteMap

import Tune (parsetune)


turnprob :: Int
turnprob = case lookupFM parsetune "turnprob" of
             Nothing -> 8
             Just v -> v

moverep :: Fragment -> Fragment
moverep = case (lookupFM parsetune "rep",lookupFM parsetune "randomrep") of
             (Just repn,Nothing) -> rep repn
             (Nothing,Just randomrepn) -> randomrep randomrepn
             (Nothing,Nothing) -> rep 4
             (Just _,Just _) -> error "must specify just one of rep and randomrep"

randomrep' :: Int -> Int -> Label -> Fragment -> Fragment
randomrep' 0 count end frag = rep (count+1) frag .*. goto end
randomrep' (n+1) count end frag = cmdFlip 2 (randomrep' n (2*count+1) end frag) .*.
                                            (randomrep' n (2*count)   end frag)

randomrep :: Int -> Fragment -> Fragment
randomrep i frag = do end <- freshLabel
                      randomrep' i 0 end frag *. label (unLabel end) skip



-- before:  what to do before each move
-- collide: what to do if move fails
-- after :  what to do after each move
walkrandomly :: String 
	     -> (Int -> Fragment) 
	     -> (Int -> Fragment) 
	     -> (Int -> Fragment) 
	     -> (Int -> LabelledFragment)
walkrandomly name before collide after dir
 = ("walkrandomly"++name++show dir) .:.
   moverep (before dir .*. ifthen cmdMove (collide dir) .*. after dir) .*.
   cmdFlip turnprob (turn (goto . walkrandomly name before collide after) dir) .*.
   goto (walkrandomly name before collide after dir)


turn :: (Int -> Fragment) -> Int -> Fragment
turn f dir = cmdFlip 2 (turnleft f dir) .*. (turnright f dir)

turnleft :: (Int -> Fragment) -> Int -> Fragment
turnleft f dir = cmdTurn Left .*. 
		 f ((dir-1) `mod` 6)

turnright :: (Int -> Fragment) -> Int -> Fragment
turnright f dir = cmdTurn Right .*. 
		  f ((dir+1) `mod` 6)
