module Compress(compress) where

import Interface
import DeepSeq

import Data.FiniteMap
import Data.Maybe


compress :: [Int] -> [VCommand] -> [VCommand]
compress steps coms = compress' newPending steps coms

compress' :: Pending -> [Int] -> [VCommand] -> [VCommand]
compress' _ [] _ = []
compress' p _ [] = flushPending p

compress' p (step:steps) (VStep step':coms) 
  | step >  step' = compress' p (step:steps) coms
  | step <= step' = flushPending p ++ [VStep step'] ++ compress' p steps coms

compress' p steps (com:coms) = (compress' $!! (updatePending p com)) steps coms

data Pending = Pending { anthill :: !(FiniteMap Pos Colour)
		       , rock :: !(FiniteMap Pos ())
		       , ant :: !(FiniteMap Pos (HasFood,Colour,Dir,Resting,AntID))
                       , noant :: !(FiniteMap Pos ())
		       , food :: !(FiniteMap Pos Amount)
		       , mark :: !(FiniteMap (Pos,Colour,MarkID) ())
		       , unmark :: !(FiniteMap (Pos,Colour,MarkID) ())
		       , score :: !(Maybe (Score,Score))
		       , comment :: !(FiniteMap AntID (Pos,String))
		       }

newPending :: Pending
newPending = Pending { anthill = emptyFM
		     , rock = emptyFM
		     , ant = emptyFM
                     , noant = emptyFM
		     , food = emptyFM
		     , mark = emptyFM
                     , unmark = emptyFM
		     , score = Nothing
		     , comment = emptyFM
		     }


instance DeepSeq Pos where
instance DeepSeq Colour where

instance DeepSeq Pending where
  deepSeq p y = deepSeq (anthill p) $ deepSeq (rock p) $ deepSeq (ant p) $
                deepSeq (noant p) $ deepSeq (food p) $ deepSeq (mark p) $
                deepSeq (mark p) $ deepSeq (unmark p) $ deepSeq (score p) $
                deepSeq (comment p) y

updatePending :: Pending -> VCommand -> Pending
updatePending p v = updatePending' p v

updatePending' :: Pending -> VCommand -> Pending
updatePending' p (VStep _) = p
updatePending' p (VAnthill col pos) = p { anthill = addToFM (anthill p) pos col }
updatePending' p (VRock pos) = p { rock = addToFM (rock p) pos () }
updatePending' p (VAnt hf col pos dir resting antid)
  = p { ant = addToFM (ant p) pos (hf,col,dir,resting,antid),
        noant = delFromFM (noant p) pos }
updatePending' p (VNoAnt pos) = p { ant = delFromFM (ant p) pos,
                                   noant = addToFM (noant p) pos () }
updatePending' p (VFood pos amount) = p { food = addToFM (food p) pos amount }
updatePending' p (VMark col pos markid) 
  = p { mark = addToFM (mark p) (pos,col,markid) (),
        unmark = delFromFM (unmark p) (pos,col,markid) }
updatePending' p (VUnMark col pos markid) 
  = p { unmark = addToFM (unmark p) (pos,col,markid) (),
        mark = delFromFM (mark p) (pos,col,markid) }
updatePending' p (VScore sc1 sc2) = p { score = Just (sc1,sc2) }
updatePending' p (VComment antid pos str) 
  = p { comment = addToFM (comment p) antid (pos,str) }
updatePending' p (VProfile _ _ _ _ _ _) = p

doanthill :: (Pos,Colour) -> VCommand
doanthill (pos,col) = VAnthill col pos

dorock :: (Pos,()) -> VCommand
dorock (pos,()) = VRock pos

doant :: (Pos,(HasFood,Colour,Dir,Resting,AntID)) -> VCommand
doant (pos,(hf,col,dir,r,antid)) = VAnt hf col pos dir r antid

donoant :: (Pos,()) -> VCommand
donoant (pos,()) = VNoAnt pos

dofood :: (Pos,Amount) -> VCommand
dofood (pos,amount) = VFood pos amount

domark :: ((Pos,Colour,MarkID),()) -> VCommand
domark ((pos,col,markid),()) = VMark col pos markid

dounmark :: ((Pos,Colour,MarkID),()) -> VCommand
dounmark ((pos,col,markid),()) = VUnMark col pos markid

doscore :: (Score,Score) -> VCommand
doscore (sc1,sc2) = VScore sc1 sc2

docomment :: (AntID,(Pos,String)) -> VCommand
docomment (antid,(pos,str)) = VComment antid pos str

flushPending :: Pending -> [VCommand]
flushPending p = out anthill doanthill ++ out rock dorock ++ out ant doant ++
		 out noant donoant ++ out food dofood ++ out mark domark ++
		 out unmark dounmark ++ out comment docomment ++
		 map doscore (maybeToList (score p))
   where out f dof = map dof (fmToList (f p))
