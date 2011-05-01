module Language (outputCommands, inputCommands, inputCommandsArray,
		 validCommands, checkCommands, mapCommand',
		 mapMCommand',
		 getComment,addCommentBegin,addCommentEnd,noComment,
		 Command, Command'(..), UCommand, UCommand'(..),
		 CommandIndex, SenseDir(..), TurnDir(..), MarkID,
		 Cond(..)) where

import Data.Array
import Control.Monad
import Interface

data SenseDir= Here | Ahead | LeftAhead | RightAhead
   deriving (Show,Read,Eq,Ord)
data TurnDir = Left | Right
   deriving (Show,Read,Eq,Ord)
data Cond = Friend | Foe | FriendWithFood | FoeWithFood | Food | Rock 
	  | Marker MarkID | FoeMarker | Home | FoeHome
   deriving (Show,Read,Eq,Ord)

data UCommand' s = Sense SenseDir s s Cond
		 | Mark MarkID s
		 | Unmark MarkID s
		 | PickUp s s
		 | Drop s
		 | Turn TurnDir s
		 | Move s s
		 | Flip Int s s
		   deriving (Show,Read,Eq,Ord)

type UCommand = UCommand' CommandIndex
newtype Command' s = C {unC :: (UCommand' s,String) }
  deriving (Show,Read)

instance Eq s => Eq (Command' s) where
  C (uc,_) == C (uc',_) = uc==uc'

instance Ord s => Ord (Command' s) where
  C (uc,_) `compare` C (uc',_) = compare uc uc'

mergeComments :: Eq s => Command' s -> Command' s -> Command' s
mergeComments (C (uc,s)) (C (uc',s')) 
 = if uc==uc' then C (uc,s++" "++s') 
     else error "trying to merge comments of different commands"

type Command = Command' CommandIndex

noComment :: UCommand' s -> Command' s
noComment uc = C (uc,"")

getComment :: Command' s -> String
getComment = snd . unC

addCommentBegin :: String -> Command' s -> Command' s
addCommentBegin str (C (uc,com)) = C (uc,str++" "++com)

addCommentEnd :: String -> Command' s -> Command' s
addCommentEnd str (C (uc,com)) = C (uc,com++" "++str)

mapUCommand' :: (a->b) -> UCommand' a -> UCommand' b
mapUCommand' f (Sense dir st1 st2 cond) = (Sense dir (f st1) (f st2) cond)
mapUCommand' f (Mark mn st) = Mark mn (f st)
mapUCommand' f (Unmark mn st) = Unmark mn (f st)
mapUCommand' f (PickUp st1 st2) = PickUp (f st1) (f st2)
mapUCommand' f (Drop st) = Drop (f st)
mapUCommand' f (Turn dir st) = Turn dir (f st)
mapUCommand' f (Move st1 st2) = Move (f st1) (f st2)
mapUCommand' f (Flip n st1 st2) = Flip n (f st1) (f st2)

mapMUCommand' :: Monad m => (a -> m b) -> UCommand' a -> m (UCommand' b)
mapMUCommand' f (Sense dir st1 st2 cond) 
  = liftM2 (\st1' st2' -> Sense dir st1' st2' cond) (f st1) (f st2)
mapMUCommand' f (Mark mn st) = liftM (Mark mn) (f st)
mapMUCommand' f (Unmark mn st) = liftM (Unmark mn) (f st)
mapMUCommand' f (PickUp st1 st2) = liftM2 PickUp (f st1) (f st2)
mapMUCommand' f (Drop st) = liftM Drop (f st)
mapMUCommand' f (Turn dir st) = liftM (Turn dir) (f st)
mapMUCommand' f (Move st1 st2) = liftM2 Move (f st1) (f st2)
mapMUCommand' f (Flip n st1 st2) = liftM2 (Flip n) (f st1) (f st2)

mapCommand' :: (a->b) -> Command' a -> Command' b
mapCommand' f (C (uc,com)) = (C (mapUCommand' f uc,com))

mapMCommand' :: Monad m => (a->m b) -> Command' a -> m (Command' b)
mapMCommand' f (C (uc,com)) = do ucb <- mapMUCommand' f uc
				 return (C (ucb,com))


outputCommands :: [Command] -> String
outputCommands []=""
outputCommands (c:cs)=outputCommand c++"\n"++outputCommands cs

outputCommand :: Command -> String
outputCommand (C (uc,com))
    = if com=="" then outputUCommand uc
       else pad 40 (outputUCommand uc) ++";"++com

pad :: Int -> String -> String
pad n str = str++replicate (n-length str) ' '

outputUCommand :: UCommand -> String
outputUCommand (Sense dir st1 st2 cond)
       ="Sense "++show dir++" "++show st1++" "++show st2++" "++show cond
outputUCommand (Mark mn state)="Mark "++show mn++" "++show state
outputUCommand (Unmark mn state)="Unmark "++show mn++" "++show state
outputUCommand (PickUp st1 st2)="PickUp "++show st1++" "++show st2
outputUCommand (Drop st)="Drop "++show st
outputUCommand (Turn dir st)="Turn "++show dir++" "++show st
outputUCommand (Move st1 st2)="Move "++show st1++" "++show st2
outputUCommand (Flip d st1 st2)="Flip "++show d++" "++show st1++" "++show st2

validCommands :: [Command] -> Bool
validCommands = all validCommand

validCommandIndex :: CommandIndex -> Bool
validCommandIndex st = 0<=st && st<=9999

validMark :: MarkID -> Bool
validMark mn = 0<=mn && mn<=5

validFlipNum :: Int -> Bool
validFlipNum n = 0<=n && n<=16383

validCommand :: Command -> Bool
validCommand (C (uc,_)) = validUCommand uc

validUCommand :: UCommand -> Bool
validUCommand (Sense _ st1 st2 cond) 
    = validCommandIndex st1 && validCommandIndex st2 && validCond cond
validUCommand (Mark mn state) = validMark mn && validCommandIndex state
validUCommand (Unmark mn state) = validMark mn && validCommandIndex state
validUCommand (PickUp st1 st2) = validCommandIndex st1 && validCommandIndex st2
validUCommand (Drop st) = validCommandIndex st
validUCommand (Turn _ st) = validCommandIndex st
validUCommand (Move st1 st2) = validCommandIndex st1 && validCommandIndex st2
validUCommand (Flip d st1 st2) 
    = validFlipNum d && validCommandIndex st1 && validCommandIndex st2

validCond :: Cond -> Bool
validCond (Marker n) = validMark n
validCond _ = True

inputCommandsArray :: String -> Array CommandIndex Command
inputCommandsArray str = listArray (0, length cs - 1) cs
    where cs = inputCommands str

inputCommands :: String -> [Command]
inputCommands str = concat (map inputCommand (lines str))

inputCommand :: String -> [Command]
inputCommand str = let (ucstr,com) = break (==';') str
                   in case inputUCommand ucstr of
                        [] -> []
                        [uc] -> [C (uc,if com=="" then "" else tail com)]
                        _ -> error "can't happen"

inputUCommand :: String -> [UCommand]
inputUCommand str 
    = case words str of
        [] -> []
        ["Sense",dir,st1,st2,cond] 
	    -> [Sense (read dir) (read st1) (read st2) (read cond)]
        ["Sense",dir,st1,st2,cond1,cond2]
          -> [Sense (read dir) (read st1) (read st2) 
	          (read (cond1++" "++cond2))]
	["Mark",mn,state] -> [Mark (read mn) (read state)]
        ["Unmark",mn,state] -> [Unmark (read mn) (read state)]
        ["PickUp",st1,st2] -> [PickUp (read st1) (read st2)]
        ["Drop",st] -> [Drop (read st)]
        ["Turn",dir,st] -> [Turn (read dir) (read st)]
        ["Move",st1,st2] -> [Move (read st1) (read st2)]
        ["Flip",d,st1,st2] -> [Flip (read d) (read st1) (read st2)]
        _ -> error ("Couldn't parse "++str)

checkCommands :: [Command] -> [Command]
checkCommands cs = if validCommands cs then cs else error "invalid commands"

-- main :: IO ()
-- main = interact (outputCommands.checkCommands.inputCommands)
