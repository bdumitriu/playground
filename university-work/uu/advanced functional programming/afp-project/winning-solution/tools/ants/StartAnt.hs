module StartAnt (mk_ant, mk_ant_all) where 

import Prelude hiding (Left,Right)
import Library
import Tune
import Data.FiniteMap

type UserProg = Fragment
type WalkUserProg = LabelledFragment

mk_ant :: [LabelledFragment] -> UserProg -> Program
mk_ant ls p = mk_ant_all ls p p p p p p

mk_ant_all :: [LabelledFragment]
           -> UserProg -> UserProg -> UserProg
           -> UserProg -> UserProg -> UserProg -> Program
mk_ant_all ls p0 p1 p2 p3 p4 p5
 = makeProgram (start p'0 p'1 p'2 p'3 p'4 p'5) $ [start p'0 p'1 p'2 p'3 p'4 p'5, ant_3_7_10 p'4 p'5, ant_4_5_8_11_12_13 p'0 p'1 p'2 p'3 p'4 p'5, ant_3, ant_7, ant_8, ant_4, ant_5, ant_2, ant_6, ant_2_6_9 p'4 p'3, ant_9 p'4 p'3, ant_9_dodge p'3, ant_10 p'4 p'5, ant_10_dodge p'5, sit, ant_14 p'3, ant_15 p'5, ant_16 p'2, ant_17 p'0, ant_18 p'1, keeper, watcher, swapover, moveout, p'0, p'1, p'2, p'3, p'4, p'5, leave4] ++ map ($ p'0) [ant_1_1, ant_1_2, ant_1_5, ant_1_6, ant_1_8, ant_1_9] ++ ls
    where p'0 = walk0 p0
          p'1 = walk1 p1
          p'2 = walk2 p2
          p'3 = walk3 p3
          p'4 = walk4 p4
          p'5 = walk5 p5

walk2 :: UserProg -> LabelledFragment
walk2 p = "SA_walk2"
   .>:. cmdMove (goto (walk2 p))
   .>*. cmdSense Here Home (goto (walk2 p))
   .>*. jump (Label "default2")

walk1 :: UserProg -> LabelledFragment
walk1 p = "SA_walk1"
   .>:. cmdMove (goto (walk1 p))
   .>*. cmdSense Here Home (goto (walk1 p))
   .>*. jump (Label "default1")

walk0 :: UserProg -> LabelledFragment
walk0 p = "SA_walk0"
   .>:. invertft (cmdSense Here Home) (goto (Label "default0"))
   .>*. cmdMove (goto (walk0 p))
   .>*. jump (walk0 p)

walk3 :: UserProg -> LabelledFragment
walk3 p = "SA_walk3"
   .>:. cmdMove (goto (walk3 p))
   .>*. cmdSense Here Home (goto (walk3 p))
   .>*. jump (Label "default3")

walk4 :: UserProg -> LabelledFragment
walk4 p = "SA_walk4"
   .>:. invertft (cmdSense Here Home) (goto (Label "default4"))
   .>*. cmdMove (goto (walk4 p))
   .>*. jump (walk4 p)

walk5 :: UserProg -> LabelledFragment
walk5 p = "SA_walk5"
   .>:. cmdMove (goto (walk5 p))
   .>*. cmdSense Here Home (goto (walk5 p))
   .>*. jump (Label "default5")

{-
ant 1 is the head ant and moves off and is responsible for doing anything
we want after the exit.

ants 2,3,4,5,6,7 are in position and just sit there

ant 8 starts on the food spot but then takes up position as the exit guard

ant 9s and 12s will leave in direction 4 and leave markers for direction 0
ant 10s and 13s will leave in direction 4 and leave markers for direction 2

ant 11s are currently the rest

ant14s will leave in direction 3 and leave markers for direction 5
ant15s will leave in direction 5 and leave markers for direction 3

ant16s will leave in direction 2 and leave markers for direction 5
ant17s will leave in direction 0 and leave markers for direction 3

ant18s go in direction 1 and leave markers for direction 4
-}

start :: WalkUserProg -> WalkUserProg -> WalkUserProg
      -> WalkUserProg -> WalkUserProg -> WalkUserProg -> LabelledFragment
start p0 p1 p2 p3 p4 p5 = "SA_start"
    .>:. cmdTurn Left
    .>*. cmdTurn Left
    -- t2
    .>*. cmdSense Ahead Friend (goto (ant_4_5_8_11_12_13 p0 p1 p2 p3 p4 p5))
    -- t3
    .>*. cmdSense LeftAhead Friend (goto (ant_3_7_10 p4 p5))
    -- we are 1,2,6,9
    -- t4
    .>*. cmdSense RightAhead Friend (goto (ant_2_6_9 p4 p3))
    -- we are 1
    -- t5
    .>*. cmdMark 1
    .>*. cmdTurn Left
    .>*. jump (ant_1_1 p0)

ant_1_1 :: WalkUserProg -> LabelledFragment
ant_1_1 p2 = "SA_ant_1_1"
       .>:. cmdMove (goto (ant_1_1 p2))
       .>*. cmdMark 0
       .>*. cmdMark 1
       .>*. cmdUnmark 2
       .>*. cmdTurn Left
       .>*. jump (ant_1_2 p2)

ant_1_2 :: WalkUserProg -> LabelledFragment
ant_1_2 p2 = "SA_ant_1_2"
       .>:. cmdMove (goto (ant_1_2 p2))
       .>*. cmdMark 0
       .>*. cmdMark 1
       .>*. cmdUnmark 2
       .>*. cmdTurn Left
       .>*. cmdTurn Left
       .>*. cmdTurn Left
       .>*. jump (ant_1_5 p2)

{-
ant_1_3 :: WalkUserProg -> LabelledFragment
ant_1_3 p2 = "SA_ant_1_3"
       .>:. cmdMove (goto (ant_1_3 p2))
       .>*. cmdMark 2
       .>*. cmdMark 1
       .>*. cmdUnmark 2
       .>*. jump (ant_1_4 p2)

ant_1_4 :: WalkUserProg -> LabelledFragment
ant_1_4 p2 = "SA_ant_1_4"
       .>:. cmdMove (goto (ant_1_4 p2))
       .>*. jump (ant_1_5 p2)
-}

ant_1_5 :: WalkUserProg -> LabelledFragment
ant_1_5 p2 = "SA_ant_1_5"
       .>:. cmdMove (goto (ant_1_5 p2))
       .>*. jump (ant_1_6 p2)

ant_1_6 :: WalkUserProg -> LabelledFragment
ant_1_6 p2 = "SA_ant_1_6"
       .>:. cmdMove (goto (ant_1_6 p2))
       .>*. cmdTurn Right
       .>*. cmdMark 0
       .>*. cmdUnmark 1
       .>*. cmdMark 2
       .>*. jump (ant_1_8 p2)

ant_1_8 :: WalkUserProg -> LabelledFragment
ant_1_8 p2 = "SA_ant_1_8"
       .>:. cmdMove (goto (ant_1_8 p2))
       .>*. cmdMark 0
       .>*. cmdUnmark 1
       .>*. cmdMark 2
       .>*. jump (ant_1_9 p2)

ant_1_9 :: WalkUserProg -> LabelledFragment
ant_1_9 p2 = "SA_ant_1_9"
       .>:. cmdMove (goto (ant_1_9 p2))
       .>*. cmdMark 0
       .>*. cmdUnmark 1
       .>*. cmdMark 2
       .>*. jump p2

ant_3_7_10 :: WalkUserProg -> WalkUserProg -> LabelledFragment
ant_3_7_10 p p' = "SA_ant_3_7_10"
        -- t4
        .>:. cmdWasteTime -- 2,6,9 are being separated from 1
        -- t5
        .>*. cmdSense LeftAhead (Marker 1) (goto ant_3)
        -- we are 10
        .>*. cmdSense LeftAhead (Marker 2) (goto ant_7)
        -- we are 10
        .>*. jump (ant_10 p p')

ant_4_5_8_11_12_13 :: WalkUserProg -> WalkUserProg -> WalkUserProg
                   -> WalkUserProg -> WalkUserProg -> WalkUserProg -> LabelledFragment
ant_4_5_8_11_12_13 p0 p1 p2 p3 p4 p5 = "SA_ant_4_5_8_11_12_13"
           -- t3
           .>:. cmdWasteTime
           -- t4
           .>*. cmdWasteTime
           -- t5
           .>*. cmdSense Ahead (Marker 1) (goto ant_8)
           -- t6
           .>*. cmdSense Ahead (Marker 1) (goto (ant_18 p1)) -- Find the one behind 8
           -- we are 4,5,11_12_13
           -- t7
           .>*. cmdSense RightAhead (Marker 2) (goto ant_4)
           -- we are 5,11_12_13
           -- t8
           .>*. cmdSense LeftAhead (Marker 1) (goto ant_5)
           -- we are 11_12_13
           -- t9
           .>*. cmdSense Ahead (Marker 2) (goto (ant_12 p4 p3))
           -- we are 11_13
           -- t10
           .>*. cmdSense Ahead (Marker 1) (goto (ant_13 p4 p5))
           -- we are 11
           -- t11
           .>*. cmdSense Ahead (Marker 2) (goto (ant_14 p3))
           -- t12
           .>*. cmdSense Ahead (Marker 1) (goto (ant_15 p5))
           -- t13
           .>*. cmdSense Ahead (Marker 2) (goto (ant_14 p3))
           -- t14
           .>*. cmdSense Ahead (Marker 1) (goto (ant_15 p5))
           -- t15
           .>*. cmdSense Ahead (Marker 2) (goto (ant_16 p2))
           -- t16
           .>*. cmdSense Ahead (Marker 1) (goto (ant_17 p0))
           -- t17
           .>*. cmdSense Ahead (Marker 2) (goto (ant_16 p2))
           -- t18
           .>*. cmdSense Ahead (Marker 1) (goto (ant_17 p0))
           -- t19
           .>*. jump (ant_18 p1)

ant_18 :: WalkUserProg -> LabelledFragment
ant_18 p = "SA_ant_18"
    .>:. cmdMark 0
    .>*. cmdTurn Left
    .>*. cmdTurn Left
    .>*. cmdTurn Left
    .>*. jump p

ant_2_6_9 :: WalkUserProg -> WalkUserProg -> LabelledFragment
ant_2_6_9 p p' = "SA_ant_2_6_9"
        -- t5
        .>:. cmdSense RightAhead (Marker 1) (goto ant_2)
        -- we are 6,9
        .>*. cmdSense RightAhead (Marker 2) (goto ant_6)
        -- we are 9
        .>*. jump (ant_9 p p')

ant_12 :: WalkUserProg -> WalkUserProg -> LabelledFragment
ant_12 = ant_9

ant_9 :: WalkUserProg -> WalkUserProg -> LabelledFragment
ant_9 p p' = "SA_ant_9"
    .>:. cmdMark 2
    .>*. cmdMark 0
    .>*. cmdMove (goto (ant_9_dodge p'))
    .>*. jump p

ant_9_dodge :: WalkUserProg -> LabelledFragment
ant_9_dodge p = "SA_ant_9_dodge"
           .>:. cmdTurn Left
           .>*. jump p

ant_10_dodge :: WalkUserProg -> LabelledFragment
ant_10_dodge p = "SA_ant_10_dodge"
            .>:. cmdTurn Right
            .>*. jump p

ant_13 :: WalkUserProg -> WalkUserProg -> LabelledFragment
ant_13 = ant_10

ant_14 :: WalkUserProg -> LabelledFragment
ant_14 p = "SA_ant_14"
    .>:. cmdMark 2
    .>*. cmdMark 0
    .>*. cmdTurn Left
    .>*. jump p

ant_15 :: WalkUserProg -> LabelledFragment
ant_15 p = "SA_ant_15"
    .>:. cmdMark 1
    .>*. cmdMark 0
    .>*. cmdTurn Right
    .>*. jump p

ant_16 :: WalkUserProg -> LabelledFragment
ant_16 p = "SA_ant_16"
    .>:. cmdMark 2
    .>*. cmdTurn Left
    .>*. cmdTurn Left
    .>*. jump p

ant_17 :: WalkUserProg -> LabelledFragment
ant_17 p = "SA_ant_17"
    .>:. cmdMark 1
    .>*. cmdTurn Right
    .>*. cmdTurn Right
    .>*. jump p

ant_10 :: WalkUserProg -> WalkUserProg -> LabelledFragment
ant_10 p p' = "SA_ant_10"
    .>:. cmdMark 1
    .>*. cmdMark 0
    .>*. cmdMove (goto (ant_10_dodge p'))
    .>*. jump p

ant_2 :: LabelledFragment
ant_2 = "SA_ant_2"
    .>:. cmdMark 2
    .>*. jump sit

ant_6 :: LabelledFragment
ant_6 = "SA_ant_6"
   .>:. cmdMark 2
   .>*. jump sit

ant_3 :: LabelledFragment
ant_3 = "SA_ant_3"
    .>:. cmdMark 2
    .>*. jump sit

ant_7 :: LabelledFragment
ant_7 = "SA_ant_7"
   .>:. cmdMark 1
   .>*. jump sit

ant_8 :: LabelledFragment
ant_8 = "SA_ant_8"
    .>:. cmdMark 1
    .>*. cmdMark 2
    .>*. cmdMark 0
    .>*. jump keeper

keeper :: LabelledFragment
keeper = "keeper"
    .>:. cmdMove (goto keeper)
    .>*. cmdTurn Right
    .>*. cmdTurn Right
    .>*. cmdTurn Right
    .>*. jump watcher

watcher :: LabelledFragment
watcher = "SA_watcher"
     .>:. cmdSense Ahead Friend (goto swapover)
     .>*. jump watcher

swapover :: LabelledFragment
swapover = "SA_swapover"
      .>:. cmdTurn Right
      .>*. cmdTurn Right
      .>*. cmdTurn Right
      .>*. rep 17 cmdWasteTime
      .>*. jump moveout

moveout :: LabelledFragment
moveout = "SA_moveout"
     .>:. cmdMove (goto moveout)
     .>*. jump (Label "SA_leave_4")

leaveBait :: Bool
leaveBait = case lookupFM parsetune "leavebait" of
                   Just _  -> True
                   Nothing -> False

ant_4 :: LabelledFragment
ant_4 = 
 if not leaveBait then
        "SA_ant_4"
    -- t8
   .>:. cmdMark 2
   .>*. jump sit
 else
        "SA_ant_4"
    -- t8
   .>:. cmdMark 2
   -- this ant is helping setting up the trap
   .>*. cmdTurn Right
   .>*. cmdTurn Right
   .>*. [forever (
           cmdSense Ahead Food ( -- we allow food drops twice
             forever (
               invertft (cmdSense Ahead Food) (
                 forever (
                   cmdSense Ahead Food (   cmdMark 0
                                       .*. cmdTurn Left
                                       .*. cmdTurn Left -- just looks nicer
                                       .*. goto sit
                                       )
                   )
                 )
               )
             )
           )
        ]

ant_5 :: LabelledFragment
ant_5 = "SA_ant_5"
    -- t9
   .>:. cmdMark 1
   .>*. jump sit

sit :: LabelledFragment
sit = "SA_sit"
  .>:. cmdWasteTime
  .>*. jump sit

leave4 :: LabelledFragment
leave4 = "SA_leave_4"
    .>:. cmdFlip 2 leave_left
    .>*. leave_right
    .>*. []

leave_right :: Fragment
leave_right = cmdTurn Right
          .*. cmdTurn Right
          .*. goto (Label "leaveR0")

leave_left :: Fragment
leave_left = cmdTurn Left
         .*. cmdTurn Left
         .*. goto (Label "leaveL2")

