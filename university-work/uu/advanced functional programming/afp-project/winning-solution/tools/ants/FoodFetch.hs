module FoodFetch where 

import Prelude hiding (Left,Right)
import Library

main :: IO ()
main = putStr $ compile ant

ant :: Program
ant = makeProgram setup $ [setup,move,move2,loop]




alldirs :: [Int -> LabelledFragment] -> [LabelledFragment]
alldirs [] = []
alldirs (f:fs) = map f [0..5] ++ alldirs fs

loop :: LabelledFragment
loop = "loop" .:. forever cmdWasteTime

setup :: LabelledFragment
setup = "setup" .:. 
        rep 2 (cmdTurn Right) .*.
        cmdSense LeftAhead Friend (goto loop) .*.
        cmdSense RightAhead Friend (goto loop) .*.
        cmdSense Ahead Friend (goto loop) .*.
        goto move

move :: LabelledFragment
move = "move" .:. 
        rep 4 (cmdMove (goto loop)) .*.
        cmdTurn Right .*.
        rep 4 (cmdMove (goto loop)) .*.
        cmdPickUp (goto move2) .*.
        goto loop

move2 :: LabelledFragment
move2 = "move2" .:.
        rep 3 (cmdTurn Right) .*.
        rep 4 (cmdMove (goto loop)) .*.
        cmdTurn Left .*.
        rep 4 (cmdMove (goto loop)) .*.
        cmdDrop .*.
        rep 3 (cmdTurn Left) .*.
        goto move


rearguard :: LabelledFragment
rearguard = "rearguard" .:. 
            rep 3 (cmdTurn Left) .*.
            goto (startsearching 3)

frontier :: LabelledFragment
frontier = "frontier" .:.
	   cmdSense LeftAhead Home (goto frontierRight) .*. 
	   goto frontierHeadOrLeft

frontierRight :: LabelledFragment
frontierRight = "frontierRight" .:. goto loop

frontierHeadOrLeft :: LabelledFragment
frontierHeadOrLeft = "frontierHeadOrLeft" .:.
		     cmdSense RightAhead Home (goto frontierLeft) .*.
		     goto frontierHead

frontierLeft :: LabelledFragment
frontierLeft = "frontierLeft" .:. goto loop

frontierHead :: LabelledFragment
frontierHead = "frontierHead" .:. goto loop

turnprob :: Int
turnprob = 8

moverep :: Int
moverep = 4

startsearching :: Int -> LabelledFragment
startsearching dir = ("startsearching"++show dir) .:.
		     markHomeRoute dir .*.
		     goto (searching dir)

searching :: Int -> LabelledFragment
searching dir = ("searching"++show dir) .:. 
		rep moverep (searchmovingcommands dir) .*.
		cmdFlip turnprob (turn searching dir) .*.
		goto (searching dir)

followingpathhome :: Int -> LabelledFragment
followingpathhome dir = ("followingpathhome"++show dir) .:.
                        cmdMove (turn followingpathhome dir) .*.
                        readMarker 0 dir (goto.followingpathhome)
                                         (goto (havefoodnopath dir)) 
                                         (goto (dropfood dir))


searchmovingcommands :: Int -> Fragment
searchmovingcommands dir = cmdPickUp (goto (justgotfood dir)) .*.
		           markHomeRoute dir .*.
		           cmdMove (turn searching dir)

justgotfood :: Int -> LabelledFragment
justgotfood dir = ("justgotfood"++show dir) .:. 
		  rep 3 (cmdTurn Left) .*.
		  goto (havefoodnopath ((dir+3) `mod` 6))

havefoodnopath :: Int -> LabelledFragment
havefoodnopath dir = ("havefoodnopath"++show dir) .:.
	             rep moverep (foodnopathmovingcommands dir) .*.
                     cmdFlip turnprob (turn havefoodnopath dir) .*.
                     goto (havefoodnopath dir)

foodnopathmovingcommands :: Int -> Fragment
foodnopathmovingcommands dir 
    = do cont <- freshLabel
         readMarker 0 dir (goto.followingpathhome) 
                          (goto cont) 
                          (goto (dropfood dir))
         label (unLabel cont) $ cmdMove (turn havefoodnopath dir)
               

dropfood :: Int -> LabelledFragment
dropfood dir = ("dropfood"++show dir) .:.
               cmdDrop .*.
               goto (startsearching dir)

turn :: (Int -> LabelledFragment) -> Int -> Fragment
turn f dir = cmdFlip 2 (turnleft f dir) .*. (turnright f dir)

turnleft :: (Int -> LabelledFragment) -> Int -> Fragment
turnleft f dir = 
		 cmdTurn Left .*. 
		 goto (f ((dir-1) `mod` 6))

turnright :: (Int -> LabelledFragment) -> Int -> Fragment
turnright f dir = cmdTurn Right .*. 
		  goto (f ((dir+1) `mod` 6))

markHomeRoute :: Int -> Fragment
markHomeRoute dir = makeMarker 0 (((dir+3) `mod` 6)+1)

makeMarker :: Int -> Int -> Fragment
makeMarker base val 
    = let (two,one,zero) = getBits val
      in
         if two==1 then cmdMark (base+2) else cmdUnmark (base+2) .*.
         if one==1 then cmdMark (base+1) else cmdUnmark (base+1) .*.
         if zero==1 then cmdMark base else cmdUnmark base

readMarker :: Int 
	   -> Int 
	   -> (Int -> Fragment) 
	   -> Fragment -> Fragment
	   -> Fragment

readMarker base dir continue nomarker abort
 = fHR2
   where fHR2   = cmdSense Here (Marker (base+2)) (fHR1 4) .*. (fHR1 0)
         fHR1 n = cmdSense Here (Marker (base+1)) (fHR0 (n+2)) .*. (fHR0 n)
         fHR0 n = cmdSense Here (Marker (base+0)) (newdir (n+1)) .*. (newdir n)
         newdir 0 = nomarker
         newdir 7 = abort
         newdir n = turnBy (((n-1)-dir) `mod` 6) .*. continue (n-1)

turnBy :: Int -> Fragment
turnBy 0 = skip
turnBy 1 = cmdTurn Right
turnBy 2 = cmdTurn Right .*. cmdTurn Right
turnBy 3 = cmdTurn Right .*. cmdTurn Right .*. cmdTurn Right
turnBy 4 = cmdTurn Left .*. cmdTurn Left
turnBy 5 = cmdTurn Left
turnBy n = error ("tried to turn by "++show n)

getBits :: Int -> (Int,Int,Int)
getBits 0 = (0,0,0)
getBits 1 = (0,0,1)
getBits 2 = (0,1,0)
getBits 3 = (0,1,1)
getBits 4 = (1,0,0)
getBits 5 = (1,0,1)
getBits 6 = (1,1,0)
getBits 7 = (1,1,1)
getBits n = error ("tried to convert "++show n++" into bits")
