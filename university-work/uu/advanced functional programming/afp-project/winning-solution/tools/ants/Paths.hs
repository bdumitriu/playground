module Paths(pathsFrags,findFood,foundFood) where

import Prelude hiding (Left,Right)
import Library
import Debug.Trace

import Marks (intMarkToDir,dirToIntMark,Direction(..))
import Walk (walkrandomly)

import Tune
import Data.FiniteMap

import Markers

enable_safety :: Bool
enable_safety = True

pathsFrags :: [LabelledFragment]
pathsFrags = spindizzily':findFoodFrags ++ foundFoodFrags ++ anywayFrags

findFoodFrags :: [LabelledFragment]
findFoodFrags = makeDirFrags ([findFood,findLocalFood
                             ,wrFindFood
                             ,followPathToFood,moveTowardsFood,moveButFinallyEscape
                             ,goAround,goOtherWayAround,randomEscape,escapeLeft,escapeRight] ++ (if enable_safety then [followPathHomeAnywayEmergency, followPathHome'Emergency True, followPathHome'Emergency False, followPathHomeDeleteEmergency] else []))

findFood :: Int -> LabelledFragment
findFood dir = "findfood"++show dir .:. 
               goto (Label ("walkrandomlyfindfood"++show dir))

-- this is only called during an emergency escape
deliverFood :: Int -> LabelledFragment
deliverFood dir = "deliverfood"++show dir .:.
                  goto (Label ("walkrandomlydeliverfood"++show dir))

-- look around for food; if none is there, delete path;
-- initial direction is the one we come from
findLocalFood :: Int -> LabelledFragment
findLocalFood dir = "findlocalfood"++show dir .:.
                    turnBy 1 .*.
                    cmdSense RightAhead Food (writeAndFollow (rightBackDir dir) (rightDir dir)) .*.
                    cmdSense Ahead Food (writeAndFollow (rightDir dir) (rightDir dir)) .*.
                    cmdSense LeftAhead Food (writeAndFollow dir (rightDir dir)) .*.
                    turnBy 3 .*.
                    cmdSense RightAhead Food (writeAndFollow (leftDir dir) (leftBackDir dir)) .*.
                    cmdSense Ahead Food (writeAndFollow (leftBackDir dir) (leftBackDir dir)) .*.
                    turnBy 5 .*.
                    -- no food present
                    removeFoodMarker .*.  -- remove special marker
                    goto (moveTowardsHomeDelete (reverseDir dir))
                                              
writeAndFollow :: Int -> Int -> Fragment
writeAndFollow fooddir dir =  writeDir FoodMarker fooddir .*.
                              goto (followPathToFood dir)

dontVisitFoeHome :: Int -> Fragment
dontVisitFoeHome = case lookupFM parsetune "dontvisitfoehome" of
		     Nothing -> (\_ -> skip)
		     Just _ -> trace "notvisiting the FoeHome"
                                       (\dir -> cmdSense Ahead FoeHome 
                                          (cmdFlip 2 (continueFindFood dir 1) .*. (continueFindFood dir (-1))))


-- Walk randomly; after each move, mark the route home, and
-- try to pickup food. If that succeeds, go home.
wrFindFood :: Int -> LabelledFragment
wrFindFood  = walkrandomly "findfood"
                           dontVisitFoeHome
                           -- collision:
                           (\dir -> cmdSense Ahead Foe
                                       (cmdFlip 2 (cmdFlip 6 (continueFindFood dir 1) .*.
                                                             (continueFindFood dir 2)) .*.
                                                  (cmdFlip 6 (continueFindFood dir (-1)) .*.
                                                             (continueFindFood dir (-2)))) .*.
                                    cmdSense Ahead FriendWithFood
                                    -- we should wait and join the friend's path
                                       (turnBy 1 .*. goto (goAround ((dir + 1) `mod` 6))) .*.
                                    cmdSense Ahead Friend
                                       (cmdTurn Right .*.
                                        goto (findFood ((dir + 1) `mod` 6))) .*.
                                    cmdSense Ahead Rock
                                       (cmdFlip 2 (cmdFlip 2 (continueFindFood dir 1) .*.
                                                             (continueFindFood dir 2)) .*.
                                                  (cmdFlip 2 (continueFindFood dir (-1)) .*.
                                                             (continueFindFood dir (-2)))))
                           -- after:
			   wrfindfoodafter

wrfindfoodafter :: Int -> Fragment
wrfindfoodafter = case lookupFM parsetune "wrfindfoodafter" of
                    Nothing -> wrfindfoodafter2
                    Just 0 -> wrfindfoodafter0
                    Just 1 -> wrfindfoodafter1
                    Just 2 -> wrfindfoodafter2
		    Just _ -> error "bad argument to wrfindfoodafter"

-- read home marker, look for food marker
wrfindfoodafter0 :: Int->Fragment
wrfindfoodafter0 dir = label ("default"++show dir) $ wrfindfoodafteronbase dir

-- check if not home, look for home marker, look for food marker
wrfindfoodafter1 :: Int->Fragment
wrfindfoodafter1 dir =              do end <- freshLabel
                                       cmdSense Here Home 
                                            (wrfindfoodafteronbase dir .*. goto end)
 			                .*. (label ("default"++show dir) $
 					          wrfindfoodafteroffbase dir)
			                .*.
                                            label (unLabel end) skip

-- check if not home, look for food marker, look for home marker
wrfindfoodafter2 :: Int->Fragment
wrfindfoodafter2 dir =              do end <- freshLabel
                                       cmdSense Here Home 
                                            (wrfindfoodafteronbase dir .*. goto end)
 			                .*. (label ("default"++show dir) $
 					          wrfindfoodafteroffbase' dir) 
			                .*.
                                            label (unLabel end) skip

wrfindfoodafteronbase :: Int -> Fragment
wrfindfoodafteronbase dir = 
                                    do end <- freshLabel
                                       readDir HomeMarker
                                           ( -- home marker present, either follow food
                                             -- or pickup food (or continue walk)
                                            const $ hasMarker FoodMarker
                                                      (goto (followPathToFood dir)) .*.
                                                    considerGoingHomeAnyway dir .*.
                                                    goto end)
                                           ( -- no marker, not been here before
                                             writeNewDir HomeMarker (reverseDir dir) .*.
                                             goto end)
                                           ( -- base, goto keeper
                                             goto (Label "keeper")) *.
                                         label (unLabel end) skip .*.
                                         cmdPickUp (goto (foundFood dir))

-- 
wrfindfoodafteroffbase :: Int -> Fragment
wrfindfoodafteroffbase dir = 
                                    do end <- freshLabel
                                       (hasMarker HomeMarker
                                           ( -- home marker present, either follow food
                                             -- or pickup food (or continue walk)
                                            hasMarker FoodMarker
                                                      (goto (followPathToFood dir)) .*.
                                                    considerGoingHomeAnyway dir .*.
                                                    goto end) .*.
                                           ( -- no marker, not been here before
                                             writeNewDir HomeMarker (reverseDir dir) .*.
                                             goto end)) *.
                                         label (unLabel end) skip .*.
                                         cmdPickUp (goto (foundFood dir))


wrfindfoodafteroffbase' :: Int -> Fragment
wrfindfoodafteroffbase' dir =       do end <- freshLabel
                                       (hasMarker FoodMarker
                                                    (goto (followPathToFood dir)) .*.
	                                hasMarker HomeMarker (considerGoingHomeAnyway dir .*. 
                                                              goto end) .*.
                                        writeNewDir HomeMarker (reverseDir dir) .*.
                                        label (unLabel end) skip .*.
                                        cmdPickUp (goto (foundFood dir)))


-- Walk randomly having food; if a route is already there, follow it home
-- We do not(!) mark the way home, because that might lead others to the
-- blockade ...
wrDeliverFood :: Int -> LabelledFragment
wrDeliverFood = walkrandomly "deliverfood"
                           (\dir -> skip)  -- before, not used
                           -- collision:
                           (\dir -> skip)
                           -- after:
                           (\dir -> hasMarker HomeMarker
                                      (goto $ followPathHomeDontMark dir))


continueFindFood :: Int -> Int -> Fragment
continueFindFood dir n = turnBy (n `mod` 6) .*. goto (findFood ((dir + n) `mod` 6))


anywayFrags :: [LabelledFragment]
anywayFrags = makeDirFrags [followPathHomeAnyway,moveTowardsHomeAnyway,
			    homeAheadAnyway,leftAnyway,rightAnyway]

gohomeanywayprob :: Int
gohomeanywayprob = case lookupFM parsetune "gohomeanywayprob" of
                     Nothing -> 1000
                     Just n -> n

-- consider going home "anyway"
considerGoingHomeAnyway :: Int -> Fragment
considerGoingHomeAnyway dir
  = cmdFlip gohomeanywayprob (goto (followPathHomeAnyway dir))

-- follow a path home "anyway"
followPathHomeAnyway :: Int -> LabelledFragment
followPathHomeAnyway dir 
  = "followpathhomeanyway"++show dir .:.
    readDir HomeMarker 
            (\newdir -> 
                        turnFromTo dir newdir .*. goto (moveTowardsHomeAnyway newdir)
            )
            (goto (findFood dir))   -- go back to exploring
            (if enable_safety then
            (
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHomeAnywayEmergency dir)) .*.
        cmdTurn Left .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHomeAnywayEmergency (leftDir dir))) .*.
        cmdTurn Left .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHomeAnywayEmergency (leftBackDir dir))) .*.
        rep 3 (cmdTurn Right) .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHomeAnywayEmergency (rightDir dir))) .*.
        cmdTurn Right .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHomeAnywayEmergency (rightBackDir dir))) .*.
        rep 2 (cmdTurn Left) .*.
        goto (followPathHomeAnywayEmergency dir)
        ) else
        spindizzily)             -- shouldn't end up on the keeper

followPathHomeAnywayEmergency :: Int -> LabelledFragment
followPathHomeAnywayEmergency dir
 = "followpathhomeanywayemergency"++show dir .>:.
   cmdMove (goto (followPathHomeAnywayEmergency dir)) .>*.
   jump (followPathHomeAnyway dir)

-- move towards home
moveTowardsHomeAnyway :: Int -> LabelledFragment
moveTowardsHomeAnyway dir
 = "movetowardshomeanyway"++show dir .:.
   cmdSense Ahead Home (goto (homeAheadAnyway dir)) .*.
   cmdMove ( -- collision
	     cmdFlip 2 (cmdTurn Left .*. goto (moveTowardsHomeAnyway (leftDir dir)))
                   .*. (cmdTurn Right .*. goto (moveTowardsHomeAnyway (rightDir dir)))
           ) .*.
   goto (followPathHomeAnyway dir)

homeAheadAnyway :: Int -> LabelledFragment
homeAheadAnyway dir
 = "homeaheadanyway"++show dir .:.
   cmdFlip 2 (goto (leftAnyway dir)) .*. (goto (rightAnyway dir))

leftAnyway :: Int -> LabelledFragment
leftAnyway dir 
    = "leftanyway"++show dir .:.
      cmdTurn Left .*.
      cmdSense Ahead Home (goto (leaveR (leftDir dir))) .*.
      goto (leftAnyway (leftDir dir))

rightAnyway :: Int -> LabelledFragment
rightAnyway dir 
    = "rightanyway"++show dir .:.
      cmdTurn Right .*.
      cmdSense Ahead Home (goto (leaveL (rightDir dir))) .*.
      goto (rightAnyway (rightDir dir))

foundFoodFrags :: [LabelledFragment]
foundFoodFrags = makeDirFrags [foundFood,foundFoodDontMark
                              ,followPathHome,followPathHomeDontMark
                              ,moveTowardsHome
                              ,moveHomeButFinallyEscape
                              ,leaveL,leaveR,wrDeliverFood,deliverFood
                              ,followPathHomeDelete,moveTowardsHomeDelete
                              ,randomEscapeHome,escapeLeftHome,escapeRightHome]


-- the setmark determines if we mark the food source as SpecialDir
-- we don't mark food sources that are *on* existing food trails
foundFood' :: Bool -> Int -> LabelledFragment
foundFood' setmark dir 
              = "foundfood"++show setmark++show dir .:.
                -- mark that we have found food
                (if setmark 
                 then (makeMarker FoodMarker (dirToIntMark SpecialDir) .*.)
                 else id)
                (turnBy 3 .*.
                 goto (moveTowardsHome (reverseDir dir)))

foundFood = foundFood' True
foundFoodDontMark = foundFood' False

-- should never happen!
spindizzily = goto spindizzily'

spindizzily' = "spin_dizzily"
           .:. forever (cmdTurn Left)


-- follow a path home
-- the boolean argument determines whether to write a food marker
-- (we don't want to if we have escaped a food trail before ...)
followPathHome' :: Bool -> Int -> LabelledFragment
followPathHome' setmarker dir 
  = "followpathhome"++show setmarker++show dir .:.
    readDir HomeMarker 
            (\newdir -> (if setmarker 
                         then (writeDir FoodMarker (reverseDir dir) .*.)
                         else id) $
                        turnFromTo dir newdir .*. goto (moveTowardsHome newdir))

            (if enable_safety then
            (
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHome'Emergency setmarker dir)) .*.
        cmdTurn Left .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHome'Emergency setmarker (leftDir dir))) .*.
        cmdTurn Left .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHome'Emergency setmarker (leftBackDir dir))) .*.
        rep 3 (cmdTurn Right) .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHome'Emergency setmarker (rightDir dir))) .*.
        cmdTurn Right .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHome'Emergency setmarker (rightBackDir dir))) .*.
        rep 2 (cmdTurn Left) .*.
        goto (followPathHome'Emergency setmarker dir)
        ) else
            spindizzily  -- no home marker should not happen!
        )
            -- backup: replace spindizzily by  goto (deliverFood dir)
            (cmdDrop .*. 
             goto (Label "keeper"))

followPathHome'Emergency :: Bool -> Int -> LabelledFragment
followPathHome'Emergency setmarker dir
 = "followpathhome'emergency"++show setmarker++show dir .>:.
   cmdMove (goto (followPathHome'Emergency setmarker dir)) .>*.
   jump (followPathHome' setmarker dir)


followPathHome = followPathHome' True
followPathHomeDontMark = followPathHome' False

-- Look around the anthill
leaveL :: Int -> LabelledFragment
leaveL dir 
  = "leaveL"++show dir .:.
    if dir /= 5 then
    ifthen (hasMarker FoodMarker) (cmdFlip 2 (goto $ followPathToFood dir)) .*.
    markRouteHome dir .*.
    invertft (cmdSense LeftAhead Home) (cmdTurn Left .*. goto (leaveL (leftDir dir))) .*.
    cmdMove (cmdTurn Right .*. cmdTurn Right .*. goto (Label ("default" ++ show (rightBackDir dir)))) .*.
    goto (leaveL dir)
    else cmdTurn Right .*. goto (Label "default0")

leaveR :: Int -> LabelledFragment
leaveR dir 
  = "leaveR"++show dir .:.
    if dir /= 3 then
    ifthen (hasMarker FoodMarker) (cmdFlip 2 (goto $ followPathToFood dir)) .*.
    markRouteHome dir .*.
    invertft (cmdSense RightAhead Home) (cmdTurn Right .*. goto (leaveR (rightDir dir))) .*.
    cmdMove (cmdTurn Left .*. cmdTurn Left .*. goto (Label ("default" ++ show (leftBackDir dir)))) .*.
    goto (leaveR dir)
    else cmdTurn Left .*. goto (Label "default2")

leaveBait :: Bool
leaveBait = case lookupFM parsetune "leavebait" of
                   Just _  -> True
                   Nothing -> False


-- move towards home
moveTowardsHome :: Int -> LabelledFragment
moveTowardsHome dir
 = "movetowardshome"++show dir .:.
   do escape <- freshLabel
      cmdMove ( -- collision
             -- goto (moveTowardsHome dir) 
             cmdSense Here Home
               ( cmdSense Ahead Foe
                   ( -- at home, we will go around enemies, but we cannot use
                     -- goAround, because that only works when not carrying food;
                     -- instead, we will do a randomEscape, which will essentially
                     -- try to surround the ant, and on an anthill should go re-enter
                     -- trail following mode almost immediately
                     goto (randomEscapeHome dir)
                   ) .*.
                 if not leaveBait then
                   goto escape
                 else
                   -- if no foe is ahead, then maybe the droppoint is ahead
                   readDirSomeWhere HomeMarker Ahead
                     (const $ goto escape) -- not interested in anything but droppoint
                     (goto escape)
                     ( -- potentially set up trap
                       cmdSense Here Food (goto escape) .*.
                       -- ask keeper if trap has already been here
                       cmdSense LeftAhead (Marker 0) (goto escape) .*.
                       -- no food, no warning, thus drop
                       cmdDrop .*. goto (findFood dir)
                     )
                 ) .*.
             -- otherwise, we try a bit longer to follow the path before
             -- escaping finally
             label (unLabel escape) (goto (moveHomeButFinallyEscape dir))
             -- TODO: distinguish friends/enemies ?
           ) .*.
        goto (followPathHome dir)

escapenum :: Int
escapenum = case lookupFM parsetune "escapenum" of
	      Nothing -> 50
              Just n -> n

-- we will try to move forward for a number of times, but if it does not
-- succeed, we will escape
moveHomeButFinallyEscape :: Int -> LabelledFragment
moveHomeButFinallyEscape dir
 = "movehomebutfinallyescape"++show dir .:.
      -- we try 50 times; because that is definitely longer than one complete "rest phase"
      -- this number definitely needs to be optimized
      rep escapenum ( invertft cmdMove (goto $ followPathHome dir) ) .*.
      -- okay, so we really cannot escape:
      -- first, we check if we are on an anthill
      cmdSense Here Home
        ( -- we check for surrounding foes
          cmdSense Ahead Foe (goto (randomEscapeHome dir)) .*.
          cmdSense LeftAhead Foe (goto (randomEscapeHome dir)) .*.
          cmdSense RightAhead Foe (goto (randomEscapeHome dir)) .*.
          turnBy 3 .*.
          cmdSense Ahead Foe (goto (randomEscapeHome $ reverseDir dir)) .*.
          cmdSense LeftAhead Foe (goto (randomEscapeHome $ reverseDir dir)) .*.
          cmdSense RightAhead Foe (goto (randomEscapeHome $ reverseDir dir)) .*.
          -- no foes around, we will drop our food with a certain chance
          -- (made chance very small, though)
          cmdFlip 50 (cmdDrop .*. goto (findFood $ reverseDir dir)) .*.
          goto (randomEscapeHome $ reverseDir dir)
        ) .*.
      -- if we're not at home, or the flip fails, we'll definitely try to escape
      goto (randomEscapeHome dir)


-- follow a path home, deleting trail
followPathHomeDelete :: Int -> LabelledFragment
followPathHomeDelete dir 
  = "followpathhomedelete"++show dir .:.
    removeFoodMarker .*.
    readDir HomeMarker 
            (\newdir -> turnFromTo dir newdir .*. goto (moveTowardsHomeDelete newdir))
            (if enable_safety then
            (
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHomeDeleteEmergency dir)) .*.
        cmdTurn Left .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHomeDeleteEmergency (leftDir dir))) .*.
        cmdTurn Left .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHomeDeleteEmergency (leftBackDir dir))) .*.
        rep 3 (cmdTurn Right) .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHomeDeleteEmergency (rightDir dir))) .*.
        cmdTurn Right .*.
        hasMarkerSomeWhere HomeMarker Ahead (goto (followPathHomeDeleteEmergency (rightBackDir dir))) .*.
        rep 2 (cmdTurn Left) .*.
        goto (followPathHomeDeleteEmergency dir)
        ) else

            spindizzily    -- no home marker shouldn't happen!
        )
            -- backup: replace spindizzily by  goto (findFood dir)
            (goto (Label "keeper"))

followPathHomeDeleteEmergency :: Int -> LabelledFragment
followPathHomeDeleteEmergency dir
 = "followpathhomedeleteemergency"++show dir .>:.
   cmdMove (goto (followPathHomeDeleteEmergency dir)) .>*.
   jump (followPathHomeDelete dir)

-- move towards home, deleting trail
moveTowardsHomeDelete :: Int -> LabelledFragment
moveTowardsHomeDelete dir
 = "movetowardshomedelete"++show dir .:.
   ifthen cmdMove ( -- collision
                    goto (findFood dir) *. skip
                    -- TODO: perhaps be more intelligent
                  ) .*.
   goto (followPathHomeDelete dir)

-- follow a path to food: first turns, then moves
--
-- if no food marker is present, we write a home pointer if none is there (new food field),
-- and try to pickup food
followPathToFood :: Int -> LabelledFragment
followPathToFood dir 
  = "followpathtofood"++show dir .:.
    readDir FoodMarker 
            (\newdir -> 
             -- the folowing line attempts to pick up food even while
             -- following a trail ...
             cmdPickUp (goto (foundFoodDontMark dir)) .*.
             turnFromTo dir newdir .*. goto (moveTowardsFood newdir))
            ( -- (hopefully) new food source
             markRouteHome dir .*.
             cmdPickUp (goto (foundFood dir)) .*. goto (findLocalFood dir))
            ( -- already existing food source
             cmdPickUp (goto (foundFood dir)) .*. goto (findLocalFood dir))

-- move towards food; first moves, then follows
moveTowardsFood :: Int -> LabelledFragment
moveTowardsFood dir
 = "movetowardsfood"++show dir .:.
   cmdMove        ( do tmp <- freshLabel
                       -- collision
                       ( cmdSense Ahead FriendWithFood
                           ( turnBy 1 .*. goto (goAround ((dir + 1) `mod` 6)) ) .*.
                         cmdSense Ahead Foe
                           ( turnBy 1 .*. goto (goAround ((dir + 1) `mod` 6)) ) .*.
                         cmdSense Ahead Friend
                           ( -- look whether we should delete the track
                             readDirSomeWhere FoodMarker Ahead
                                (const $ goto tmp)
                                ( -- no marker; assume delete
                                 goto (followPathHomeDelete dir))
                                ( -- special marker; go ahead
                                 goto tmp)
                           ) .*.
                         label (unLabel tmp) (goto (moveButFinallyEscape dir))
                        )
           ) .*.
   -- no collision, we have moved
   goto (followPathToFood dir)

-- we will try to move forward for a number of times, but if it does not
-- succeed, we will escape
moveButFinallyEscape :: Int -> LabelledFragment
moveButFinallyEscape dir
 = "movebutfinallyescape"++show dir .:.
   -- we try 20 times; because that is definitely longer than one complete "rest phase"
   rep 20 ( invertft cmdMove (goto $ followPathToFood dir) ) .*.
   goto (randomEscape dir)
   

-- going around: if right ahead is blocked, try left ahead
goAround :: Int -> LabelledFragment
goAround dir
 = "goaround"++show dir .:.
   cmdMove (turnBy 4 .*. goto (goOtherWayAround (leftBackDir dir))) .*.
   markRouteHome dir .*.
   -- before writing a food trail, look if there isn't any food here
   cmdPickUp (goto (foundFood dir)) .*. 
   markRouteToFood ((dir-2)`mod`6) .*.
   goto (followPathToFood dir)
   
-- if left ahead is blocked as well, try to escape 
goOtherWayAround :: Int -> LabelledFragment
goOtherWayAround dir
 = "gootherwayaround"++show dir .:.
   cmdMove (goto (randomEscape dir)) .*.
   markRouteHome dir .*.
   -- before writing a food trail, look if there isn't any food here
   cmdPickUp (goto (foundFood dir)) .*.
   markRouteToFood ((dir+2)`mod`6) .*.
   goto (followPathToFood dir)

-- random escape is an emergency escape
randomEscape :: Int -> LabelledFragment
randomEscape dir
 = "randomescape"++show dir .:.
   cmdFlip 2 (goto $ escapeLeft dir) .*.
   goto (escapeRight dir)

escapeLeft :: Int -> LabelledFragment
escapeLeft dir
 = "escapeleft"++show dir .:.
   turnBy 5 .*. cmdMove (goto $ escapeLeft (leftDir dir)) .*. markRouteHome (leftDir dir) .*. goto (findFood (leftDir dir))

escapeRight :: Int -> LabelledFragment
escapeRight dir
 = "escaperight"++show dir .:.
   turnBy 1 .*. cmdMove (goto $ escapeRight (rightDir dir)) .*. markRouteHome (rightDir dir) .*. goto (findFood (rightDir dir))

-- random escape is an emergency escape
randomEscapeHome :: Int -> LabelledFragment
randomEscapeHome dir
 = "randomescapehome"++show dir .:.
   cmdFlip 2 (goto $ escapeLeftHome dir) .*.
   goto (escapeRightHome dir)


escapeLeftHome :: Int -> LabelledFragment
escapeLeftHome dir
 = "escapelefthome"++show dir .:.
   turnBy 5 .*. cmdMove (goto $ escapeLeftHome (leftDir dir)) .*. goto (deliverFood (leftDir dir))

escapeRightHome :: Int -> LabelledFragment
escapeRightHome dir
 = "escaperighthome"++show dir .:.
   turnBy 1 .*. cmdMove (goto $ escapeRightHome (rightDir dir)) .*. goto (deliverFood (rightDir dir))


-- mark the route home if there isn't already a home route marker
markRouteHome :: Int -> Fragment
markRouteHome 0 = quickCheckAndMark 0 2 1
markRouteHome 1 = quickCheckAndMark 1 2 0
markRouteHome 2 = quickCheckAndMark 1 0 2
markRouteHome dir = ifthen (invertft (hasMarker HomeMarker))
                           (writeNewDir HomeMarker (reverseDir dir))
                        

-- mark the route to food if there isn't already a food route marker
markRouteToFood :: Int -> Fragment

markRouteToFood 3 = quickCheckAndMark 3 5 4
markRouteToFood 4 = quickCheckAndMark 4 5 3
markRouteToFood 5 = quickCheckAndMark 4 3 5
markRouteToFood dir = ifthen (invertft (hasMarker FoodMarker))
                             (writeNewDir FoodMarker dir)
                        
quickCheckAndMark :: Int -> Int -> Int -> Fragment
quickCheckAndMark a b c = do end <- freshLabel
                             cmdSense Here (Marker a) (goto end) .*.
                              cmdSense Here (Marker b) (goto end) .*.
                              cmdMark c .*.
                              label (unLabel end) skip

reverseDir :: Int -> Int
reverseDir dir = (dir+3) `mod` 6

leftDir :: Int -> Int
leftDir dir = (dir-1) `mod` 6

rightDir :: Int -> Int
rightDir dir = (dir+1) `mod` 6

leftBackDir :: Int -> Int
leftBackDir dir = (dir-2) `mod` 6

rightBackDir :: Int -> Int
rightBackDir dir = (dir+2) `mod` 6

readDirSomeWhere
        :: MarkerType          -- base
        -> SenseDir            -- sense direction to read from
	-> (Int -> Fragment)   -- if there is a marker
	-> Fragment            -- if there is no marker
	-> Fragment            -- if there is a special marker
	-> Fragment

readDirSomeWhere base sensedir founddir nomarker specialmarker
 = readMarkerSomeWhere base sensedir (founddirection . intMarkToDir)
       where founddirection NoDir = nomarker
	     founddirection SpecialDir = specialmarker
	     founddirection (Dir n) = founddir n

-- special case
readDir :: MarkerType -> (Int->Fragment) -> Fragment -> Fragment -> Fragment
readDir base = readDirSomeWhere base Here

-- write a marker assuming the old one was zero
writeNewDir :: MarkerType
	    -> Int
	    -> Fragment
writeNewDir base dir 
 = rewriteMarker base (dirToIntMark NoDir) (dirToIntMark (Dir dir))


writeDir :: MarkerType
	 -> Int    -- direction
	 -> Fragment
writeDir base dir = makeMarker base (dirToIntMark (Dir dir))

removeFoodMarker :: Fragment
removeFoodMarker = makeMarker FoodMarker (dirToIntMark (NoDir))

-- from and to are source and target direction
turnFromTo :: Int -> Int -> Fragment
turnFromTo from to = turnBy ((to-from) `mod` 6)

-- turn n steps to the right, but do it optimally
turnBy :: Int -> Fragment
turnBy 0 = skip
turnBy 1 = cmdTurn Right
turnBy 2 = cmdTurn Right .*. cmdTurn Right
turnBy 3 = cmdTurn Right .*. cmdTurn Right .*. cmdTurn Right
turnBy 4 = cmdTurn Left .*. cmdTurn Left
turnBy 5 = cmdTurn Left
turnBy n = error ("tried to turn by "++show n)
