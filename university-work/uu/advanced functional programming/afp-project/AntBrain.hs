-- | Here our own ant brain
-- is defined.
module Main where

import Ant
import Antcode
import Compiler hiding (goto)
import HLAntCombinators
import HLAntCode
import ExampleHLAntCode
import Prelude hiding(Left, Right, drop, break, flip, not)

-- | The main function will just output the
--   low level ant code.
main = outputCompile $ strategy

-- | Is there a path to food around me?
pathToFood3Around :: Cond
pathToFood3Around = foldr1 (.|) $ map senseNear [3..5]

-- | Sense for the given marker near me.
senseNear :: Int -> Cond
senseNear x = sense Here (Marker x) .| sense Ahead (Marker x) 
      .| sense LeftAhead (Marker x) .| sense RightAhead (Marker x)

-- | Sense for a marker to food in the given direction.
senseMarkerToFood d = sense d (Marker 3) .| sense d (Marker 4) .| sense d (Marker 5)

-- | Marks the drop point.
markDropPoint :: HLI
markDropPoint = markRaw 0 .: markRaw 1 .: markRaw 2

-- | Marks a temporary mark (used in the initialization procedure)
markTemp1 :: HLI
markTemp1 = markRaw 0

-- | Unmarks a temporary mark (used in the initialization procedure)
unmarkTemp1 :: HLI
unmarkTemp1 = unmarkRaw 0

-- | Marks a temporary mark (used in the initialization procedure)
markTemp2 :: HLI
markTemp2 = markRaw 1

-- | Unmarks a temporary mark (used in the initialization procedure)
unmarkTemp2 :: HLI
unmarkTemp2 = unmarkRaw 1

-- | Senses a temporary mark (used in the initialization procedure)
senseTemp1 :: SenseDir -> Cond
senseTemp1 d = sense d (Marker 0)

-- | Senses a temporary mark (used in the initialization procedure)
senseTemp2 :: SenseDir -> Cond
senseTemp2 d = sense d (Marker 1)

-- | Tries to move, does nothing if unsucessful.
tryMove :: HLI
tryMove = move .< nop .+ nop

-- | Try to move three times.
insistMove :: Cond
insistMove = move .| move .| move

-- | Is the drop point in the given direction?
senseDropPoint d = foldr1 (.&) $ map (\x -> sense d (Marker x)) [0..2]

-- | Is there food around me?
foodAround :: Cond
foodAround = (sense Here Food .| sense Ahead Food .| sense LeftAhead Food .| sense RightAhead Food) 
      .& (not (sense Here Home)) -- Don't get food from home!

-- | Wait for 12 turns.
wait :: HLI
wait = turnBack .: turnBack .: turnBack .: turnBack 
             
-- | Does nothing until it can move.
mOVE :: HLI
mOVE = nop .@ move

-- | Rotates from the first direction to the second one.
rotate :: Int -> Int -> HLI
rotate from to | from == to = nop
               | otherwise = if (from > to)
                             then (turnLeft .: rotate (from-1) to)
                             else (turnRight .: rotate from (to-1))

-- | Defines the starting procedure.
start :: HLI
start = unite
  [
   -- initiate marking procedures
   not (sense Ahead Friend)
    .< ((not (sense LeftAhead Friend) .& (sense RightAhead Friend))
        .< -- 5 ants above
           unite
           [
            turnRight, drop, drop,
            senseTemp1 Ahead
             .< -- Ant just above 
                unite [markTemp2, turnLeft, mOVE, markPathHome2 1, turnBack, mOVE, unmarkTemp2, drop .@ false]
             .+ -- Other 4 ants          
                unite [markPathHome2 0, turnLeft, goto (tag "Look for food and mark" 0)]
           ]
        .+ ((not (sense RightAhead Friend) .& (sense LeftAhead Friend))
            .< -- 5 ants below
               unite
               [
                turnLeft, drop, drop,
                senseTemp1 Ahead
                 .< -- Ant just below 
                    unite [markTemp2, turnRight, mOVE, markPathHome2 5, turnBack, mOVE, unmarkTemp2, drop .@ false]
                 .+ -- Other 4 ants  
                    unite [markPathHome2 0, turnRight, goto (tag "Look for food and mark" 0)]
               ]
            .+ -- The ant in front of everyone.
               unite
               [
                markTemp1,
                mOVE,
                markPathHome2 5,
                goto (tag "Look for food and mark" 0)
               ]))
    .+ -- Remaining ants
       unite
       [
       turnBack, turnBack, drop,
       senseTemp1 Ahead
        .< -- Single ant at the drop point
           unite [markDropPoint, mOVE, unmarkTemp1, turnBack, goto "Gate procedure"]
        .+ -- All others
           unite
           [
            senseTemp2 Ahead
            .< -- Two ants that never move.
               (drop .@ false)
            .+ -- All others
               unite 
               [
                turnBack,
                not (sense Ahead Friend)
                .< (not (sense LeftAhead Friend)
                    .< (not (sense RightAhead Friend)
                       .< -- Single ant at the back
                          markLine
                       .+ -- side 3 of the hex
                          (markPathHome2 2 .: turnLeft .: goFast 2 .: goto (tag "Look for food and mark" 2)))
                    .+ -- side 2 of the hex
                       (markPathHome2 4 .: turnRight .: goFast 4 .: goto (tag "Look for food and mark" 4)))
                .+ -- Others 
                   (turnLeft .:
                   not (sense Ahead Friend .| sense LeftAhead Friend)
                    .< -- side 4 of the hex
                       (markPathHome2 1 .: turnLeft .: goFast 1 .: goto (tag "Look for food and mark" 1))
                    .+ (turnRight .: turnRight .:
                        not (sense Ahead Friend .| sense RightAhead Friend)
                        .< -- side 1
                           (markPathHome2 5 .: turnRight .: goFast 5 .: goto (tag "Look for food and mark" 5))
                        .+ (rotate 4 0 .: goto (tag "Look for food and mark" 0))
                        ))
               ]
           ]
       ]
   --goto (tag "Look for food and mark" 0)
  ]

-- | Marks a line with path home (used in initialization procedure)
markLine :: HLI
markLine = unite
 [
  turnBack,
  markPathHome2 3,
  (mOVE .: markPathHome2 3) .@ (senseDropPoint Ahead),
  turnBack,
  goto (tag "Look for food and mark" 3)
 ]
  
-- | The gate procedure
gateProcedure :: HLI
gateProcedure = unite
  [
  label "Gate procedure",
  nop .@ (sense Ahead Friend),
  turnBack,
  -- to avoid clustering the exit point
  flip 2
   .< (turnLeft .: goto (tag "Find food marker around home on left" 5))
   .+ (turnRight .: goto (tag "Find food marker around home on right" 1))
  ]

goFast :: Dir -> HLI
goFast d = (markPathHome d) .@ (sense Here Food .| (not insistMove))

-- | Find food markers around home, to the right.
findFMAroundHomeR :: Dir -> HLI
findFMAroundHomeR d = unite
    [
        label $ tag "Find food marker around home on right" d,
        senseMarkerToFood Here
            .< (flip 3 .< nop .+ goto (tag "Follow path to food" d))
            .+ nop,
        -- walk around the home
        sense RightAhead Home
            .< (insistMove 
                -- go on
                .< goto (tag "Find food marker around home on right" d)
                -- collision?
                .+ (turnLeft .: markPathHome (turn Left d) .: goto (tag "Look for food and mark" (turn Left d)))
                )
            .+ (
                turnRight .:
                goto (tag "Find food marker around home on right" (turn Right d))
                )
    ]

-- | Find food markers around home, to the left.
findFMAroundHomeL :: Dir -> HLI
findFMAroundHomeL d = unite
    [
        label $ tag "Find food marker around home on left" d,
        senseMarkerToFood Here
            .< (flip 3 .< nop .+ goto (tag "Follow path to food" d))
            .+ nop,
        -- walk around the home
        sense LeftAhead Home
            .< (insistMove 
                -- go on
                .< goto (tag "Find food marker around home on left" d)
                -- collision?
                .+ (turnRight .: markPathHome (turn Right d) .: goto (tag "Look for food and mark" (turn Right d)))
                )
            .+ (
                turnLeft .:
                goto (tag "Find food marker around home on left" (turn Left d))
                )
    ]
  
-- | Given the direction the ant is currently facing,
-- this code will look for food until if finds it, marking
-- the path with integers from 0..5
lookFFandMark :: Dir -> HLI
lookFFandMark d = unite
  [
  label $ tag "Look for food and mark" d,
  -- maybe there is already a path to food around me?
  senseMarkerToFood Here
    .< goto (tag "Follow path to food" d)
    .+ nop,
  -- look for food around me
  foodAround
    -- there is food, take it
    .< goto (tag "Take nearby food" d)
    -- no food, forget
    .+ nop,

  -- become a keeper
  senseDropPoint Here
    .< (rotate d 0 .: goto (tag "Go home and don't mark" 0))
    .+ nop,
    
  -- move
  move
    -- repeat, might turn sometimes.
    .< (
        -- mark path to return home
        markPathHome d .:
        flip 20
            .< (turnRight .: goto (tag "Look for food and mark" (turn Right d)))
            .+ goto (tag "Look for food and mark" d)
        )
    -- can't move! turn somewhere and repeat
    .+ (flip 2
         .< (turnRight .: goto (tag "Look for food and mark" (turn Right d)))
         .+ (turnLeft .: goto (tag "Look for food and mark" (turn Left d)))
       )
    -- can't move! turn RIGHT and repeat
    -- .+ (turnRight .: goto (tag "Look for food and mark" (turn Right d)))
  ]

-- | Deletes a path to food.
deletePathToFood :: Dir -> HLI
deletePathToFood oldD = unite
  [
   label (tag "Delete path to food" oldD),
   -- supposedly, there is a food marker here
   sense Here (Marker 3)
    .< (sense Here (Marker 4)
        .< (sense Here (Marker 5)
            .< -- This value is not used! Shouldn't happen
               goto (tag "Look for food and mark" oldD)
            .+ proc 5) -- dir == 2
        .+ (sense Here (Marker 5)
            .< proc 1 -- dir == 4
            .+ proc 3)) -- dir == 0
    .+ (sense Here (Marker 4)
        .< (sense Here (Marker 5)
            .< proc 2 -- dir == 5
            .+ proc 4) -- dir == 1
        .+ (sense Here (Marker 5)
            .< proc 0 -- dir == 3
            .+ -- no marker, let's forget the idea
               goto (tag "Look for food and mark" oldD)))
  ]
  where
    proc newD = unite
                [
                 unmarkPathToFood,
                 rotate oldD newD,
                 insistMove
                 .< goto (tag "Delete path to food" newD)
                 -- ok, the way is blocked, forget unmarking.
                 .+ goto (tag "Look for food and mark" newD)
                ]
  
-- | Follows a path to food.
followPathToFood :: Dir -> HLI
followPathToFood oldD = unite 
  [
  label (tag "Follow path to food" oldD),
  -- Am I at food already?
  foodAround
   .< -- yes, get it
      goto (tag "Take nearby food" oldD)
   .+ -- no, continue
      nop,
  -- Read where to move
  sense Here (Marker 3)
   .< (sense Here (Marker 4)
       .< (sense Here (Marker 5)
           .< -- This value is not used! Shouldn't happen
              goto (tag "Look for food and mark" oldD)
           .+ proc 2) -- dir == 2
       .+ (sense Here (Marker 5)
           .< proc 4 -- dir == 4
           .+ proc 0)) -- dir == 0
   .+ (sense Here (Marker 4)
       .< (sense Here (Marker 5)
           .< proc 5 -- dir == 5
           .+ proc 1) -- dir == 1
       .+ (sense Here (Marker 5)
           .< proc 3 -- dir == 3
           .+ -- no marker, might be that there WAS food here.
              -- notice that I know already that
              -- there is no food ahead me, nor where
              -- I am
              unite
              [
               move
               .< (food3Around
                   .< (goto (tag "Take nearby food" oldD))
                   .+ -- ok, no food, let's delete the path
                      (turnBack .:
                       move
                        .< nop
                        -- oh well...
                        .+ goto (tag "Look for food and mark" (invert oldD))
                       ))
               .+ -- can't move? hell, delete the path anyway
                  turnBack,
               -- we are inverted, and we shall delete the path were
               -- we came from. I also know that here there is NO marker
               move
                -- so I walk back to were I came from
                .< (goto (tag "Delete path to food" (invert oldD)))
                -- if I can, of course.    
                .+ (goto (tag "Look for food and mark" (invert oldD)))
              ]))
  ]
  where
    proc newD =
        unite
        [rotate oldD newD,
        insistMove
        .< goto (tag "Follow path to food" newD)
        -- ok, the way is blocked, forget this food.
        -- Notice: The ant must really get out of the way. Otherwise it will come here again very soon.
        .+ (flip 50
            .< (
                turnBack .:
                repeatN 15 (tryMove .: markPathHome (invert newD)) .:
                goto (tag "Look for food and mark" (invert newD))
            )
            .+ (flip 2
            .< (
                turnRight .:
                tryMove .:
                markPathHome (turn Right newD) .:
                goto (tag "Look for food and mark" (turn Right newD))
            )
            .+ (
                turnLeft .:
                tryMove .:
                markPathHome (turn Left newD) .:
                goto (tag "Look for food and mark" (turn Left newD))
                )
            )
        )
        ]
               
-- | Marks a path home, given a direction.
markPathHome2 :: Dir -> HLI
markPathHome2 d = unite
  [
  -- check if there is something already marked
  sense Here (Marker 0)
   .< nop
   .+ (sense Here (Marker 1)
       .< nop
       .+ (sense Here (Marker 2)
           .< nop
           .+ unite (map markRaw (int2marks (invert d)))))
  ]

-- | Marks a path home, given a direction.
markPathHome :: Dir -> HLI
markPathHome d = unite
  [
  -- check if there is something already marked or if we are at home
  sense Here (Marker 0)
   .< nop
   .+ (sense Here (Marker 1)
       .< nop
       .+ (sense Here (Marker 2)
           .< nop
           .+ (sense Here Home .< nop .+ unite (map markRaw (int2marks (invert d))))))
  ]
  
-- | Unmarks a path home, given a direction.
unmarkPathHome :: Dir -> HLI
unmarkPathHome d = 
  unite $ map unmarkRaw (int2marks (invert d))

-- | Marks a path to food, given the direction the ant is facing.
-- Note that the path is actually the opposite of the direction.
markPathToFood :: Dir -> HLI
markPathToFood d = unite
  [
  -- check if there is something already marked, dont mark in foehome to avoid trap.
  (sense Here Home .| sense Here FoeHome)
   -- We're home, don't mark
   .< nop
   .+ ( 
      sense Here (Marker 3)
       .< nop
       .+ (sense Here (Marker 4)
           .< nop
           .+ (sense Here (Marker 5)
               .< nop
               .+ unite (map (markRaw . (+3)) (int2marks (invert d))))))
  ]

-- | Unmarks ANY path to food
unmarkPathToFood :: HLI
unmarkPathToFood = 
  unite $ map unmarkRaw [3..5]

-- | Picks food nearby, takes current direction
takeNearbyFood :: Dir -> HLI
takeNearbyFood d = unite
  [
  label $ tag "Take nearby food" d,
  (sense Here Food .& (not (sense Here Home)))
  .< (pickUp .< goto (tag "Go home and mark" d) .+ goto (tag "Look for food and mark" d))
  .+
   (sense Ahead Food .& (not (sense Ahead Home)) 
    .< goto (tag "Enter food" d)
    .+ (sense LeftAhead Food .& (not (sense LeftAhead Home)) 
        .< unite [turnLeft, goto (tag "Enter food" (turn Left d))]
        .+ (sense RightAhead Food .& (not (sense RightAhead Home)) 
           .< unite [turnRight, goto (tag "Enter food" (turn Right d))]
           -- where's the food gone to? Look for it again
           .+ (turnLeft .: goto (tag "Look for food and mark" (turn Left d)))
           )
       )
    )
  ]

-- | The ant has food. Will follow the path home
-- converting the path to "path to food".  
followPathHome :: Dir -> HLI
followPathHome oldD = unite
  [
  label $ tag "Go home and mark" oldD,
  -- Am I home already?
  senseDropPoint Here
   .< -- yes, nice, drop&go
      (drop .: mOVE .: turnBack .: goto "Gate procedure")
   .+ -- no, is the dropPoint ahead?
      (senseDropPoint Ahead .& not (sense Here Food)
       -- yes, set up a trap!!!
       .< (sense Here FoeMarker
            -- even if the enemy has already been here,
            -- we might want to drop some food...
            .< (flip 30 .< drop .+ nop)
            -- lure the enemy here
            .+ drop)
       .+ nop),
  -- Read where to move
  sense Here (Marker 0)
   .< (sense Here (Marker 1)
       .< (sense Here (Marker 2)
           .< -- This value is not used! Shouldn't happen
              goto (tag "Look for food and mark" oldD)
           .+ proc 2) -- dir == 2
       .+ (sense Here (Marker 2)
           .< proc 4 -- dir == 4
           .+ proc 0)) -- dir == 0
   .+ (sense Here (Marker 1)
       .< (sense Here (Marker 2)
           .< proc 5 -- dir == 5
           .+ proc 1) -- dir == 1
       .+ (sense Here (Marker 2)
           .< proc 3 -- dir == 3
           .+ -- no marker, that's nasty. 
              (sense Here Home 
               .< (proc oldD)
               .+ (goto (tag "Wander around looking for marker" oldD)))))
  ]
  where
    proc newD = unite
                [rotate oldD newD,
                 move
                   .< (
                       markPathToFood newD .:
                       goto (tag "Go home and mark" newD)
                   )
                   -- Can't move forward... solve collision
                   .+ (
                       turnBack .: turnBack .:
                       move
                         .< (-- yes, it went away, nice.
                            markPathToFood newD .:
                            goto (tag "Go home and mark" newD)
                         )
                         .+ goto (tag "Get out of the way" newD)
                   )
                 ]
     
-- | The ant has food. Will follow the path home
-- without marking a path to food
followPathHomeNoMark :: Dir -> HLI
followPathHomeNoMark oldD = unite
  [
  label $ tag "Go home and don't mark" oldD,
  -- Am I home already?
  senseDropPoint Here
   .< -- yes, nice, drop&go
      (drop .: mOVE .: turnBack .: goto "Gate procedure")
   .+ -- no, continue
      nop,
  -- Read where to move
  sense Here (Marker 0)
   .< (sense Here (Marker 1)
       .< (sense Here (Marker 2)
           .< -- This value is not used! Shouldn't happen
              goto (tag "Look for food and mark" oldD)
           .+ proc 2) -- dir == 2
       .+ (sense Here (Marker 2)
           .< proc 4 -- dir == 4
           .+ proc 0)) -- dir == 0
   .+ (sense Here (Marker 1)
       .< (sense Here (Marker 2)
           .< proc 5 -- dir == 5
           .+ proc 1) -- dir == 1
       .+ (sense Here (Marker 2)
           .< proc 3 -- dir == 3
           .+ -- no marker, that's nasty. 
              (sense Here Home 
               .< (proc oldD)
               .+ (goto (tag "Wander around looking for marker" oldD)))))
  ]
  where
    proc newD = unite
                [rotate oldD newD,
                 move
                   .< goto (tag "Go home and don't mark" newD)
                   -- Can't move forward... solve collision
                   .+ goto (tag "Get out of the way" newD)]

-- | Collision procedure.
getOutOfTheWay :: Dir -> HLI
getOutOfTheWay d = unite
  [
  label $ tag "Get out of the way" d,
  -- Well, I have food but there's something
  -- in front of me. I shall wait a bit first,
  -- maybe it will go away.
  turnBack .: turnBack .: turnBack .: turnBack,
  move
   .< (-- yes, it went away, nice.
       -- Why is there a "don't" below?!?
       -- introduced in version 117 by quhw
      goto (tag "Go home and don't mark" d)
   )
   -- well, there are probably other paths.
   -- Just move away and try something else, but
   -- don't mark this new path
   .+ (flip 50
        .< (
            (sense Here Home)
            -- if we are stuck at home, maybe enemies block our gate. Drop it anyway!
            .< (
                drop .:
                goto (tag "Look for food and mark" d)
            )
            .+ (
                turnBack .:  -- leave this point far away, avoid some bad stuck situation.
                repeatN 15 tryMove .:
                goto (tag "Go home and don't mark" (invert d))
            )
        )
        .+ (flip 2
            .< (turnRight .:
                (tryMove .: goto (tag "Go home and don't mark" (turn Right d))))
            .+ (turnLeft .:
                (tryMove .: goto (tag "Go home and don't mark" (turn Left d))))
         )
   )
  ]

-- | Look for marker to home, don't mark path to food.
wanderAround :: Dir -> HLI
wanderAround oldD = unite
  [
  label (tag "Wander around looking for marker" oldD),
  tryMove,
  sense Here (Marker 0)
   .< (sense Here (Marker 1)
       .< (sense Here (Marker 2)
           .< -- This value is not used! Shouldn't happen
              goto (tag "Look for food and mark" oldD)
           .+ proc 2) -- dir == 2
       .+ (sense Here (Marker 2)
           .< proc 4 -- dir == 4
           .+ proc 0)) -- dir == 0
   .+ (sense Here (Marker 1)
       .< (sense Here (Marker 2)
           .< proc 5 -- dir == 5
           .+ proc 1) -- dir == 1
       .+ (sense Here (Marker 2)
           .< proc 3 -- dir == 3
           .+ -- still no marker
              (flip 2
               .< (turnRight .: goto (tag "Wander around looking for marker" (turn Right oldD)))
               .+ (turnLeft .: goto (tag "Wander around looking for marker" (turn Left oldD))))))
  ]
  where
    proc newD = unite
                [rotate oldD newD,
                 move
                   .< goto (tag "Go home and don't mark" newD)
                   -- Can't move forward... solve collision
                   .+ goto (tag "Get out of the way" newD)]

-- | Enters nearby food.
enterFood :: Dir -> HLI
enterFood d = unite
  [
  label $ tag "Enter food" d,
  (sense LeftAhead Foe .& sense RightAhead Foe)
  -- hum... this might be a trap
   .< (
        -- get out of here
        turnBack .:
        goto (tag "Look for food and mark" (invert d))
      )
   .+ nop,
  move
   .< (
       markPathHome d .:
       pickUp
       .< unite 
          [
            -- Ok, let's turn back
            turnBack,
            goto (tag "Go home and mark" (invert d))
           ]
       -- no food? Ok... then look again for food.
       .+ goto (tag "Look for food and mark" d))
   -- can't move to food? Ok... look again for food.
   -- Notice: In some cases, the ant will get into infinite waiting. So get it away from the food.
   .+ (flip 50
        .< (turnBack .:
            repeatN 15 (tryMove .: markPathHome (invert d)) .:
            goto (tag "Look for food and mark" (invert d))
            )
        .+ (flip 2
                .< (turnRight .:
                    tryMove .:
                    markPathHome (turn Right d) .:
                    goto (tag "Look for food and mark" (turn Right d))
                    )
                .+ (turnLeft .:
                    tryMove .:
                    markPathHome (turn Left d) .:
                    goto (tag "Look for food and mark" (turn Left d))
                    )
           )
      )
   ]

-- | Overall strategy combining everything.
strategy :: HLI
strategy = unite $ (others ++ map unite (map (\f -> map f [0..5]) procs))

others = [start, gateProcedure]

procs :: [Int -> HLI]
procs = [goFast, lookFFandMark, takeNearbyFood, followPathHome, enterFood,
        followPathToFood, getOutOfTheWay, wanderAround, followPathHomeNoMark,
        deletePathToFood, findFMAroundHomeL, findFMAroundHomeR]

-- | Tags the 'String' with the given 'Int'
tag :: String -> Int -> String
tag s n = s ++ (show n)