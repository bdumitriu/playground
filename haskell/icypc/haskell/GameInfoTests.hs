module GameInfoTests where

import Test.HUnit

import GameStateParser
import GameInfo

{-
, emptyGameInfo
, registerNewState
, currentState
, currentBareState
-}

stateAtTurn0 = parseGameState . unlines $ [
    "0",
    "0 0",
    "3a 3a 3a 3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "0 2 S a 0",
    "1 2 S a 0",
    "2 1 S a 0",
    "2 0 S a 0",
    "*",
    "*",
    "*",
    "*"]

testRegisterFirstState = TestCase (do let gameInfo  = registerNewState emptyGameInfo stateAtTurn0
                                          f         = field state
                                          reds      = redChildren state
                                          state     = currentState gameInfo
                                          bareState = currentBareState gameInfo
                                      assertEqual "bare state is registered state" stateAtTurn0 bareState
                                      assertEqual "augmented state in first turn has info age 0 for all known field spaces" True (checkAllInfoAgesAreZero f)
                                      assertEqual "red child 1 in field @ 0 2" (getChild1 reds) (childIn (f!(0, 2)))
                                      assertEqual "red child 2 in field @ 1 2" (getChild2 reds) (childIn (f!(1, 2)))
                                      assertEqual "red child 3 in field @ 2 1" (getChild3 reds) (childIn (f!(2, 1)))
                                      assertEqual "red child 4 in field @ 2 0" (getChild4 reds) (childIn (f!(2, 0)))
                                      assertEqual "no child in field @ other position" Nothing (childIn (f!(12, 10)))
                                  )

checkAllInfoAgesAreZero :: Field -> Bool
checkAllInfoAgesAreZero = null . filter (\fieldSpace -> infoAge fieldSpace /= 0) . elems

tests = TestList [
           TestLabel "testRegisterFirstState" testRegisterFirstState
        ]

main = runTestTT tests
