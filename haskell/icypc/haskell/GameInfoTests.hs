module GameInfoTests where

import Test.HUnit

import GameStateParser
import GameInfo

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

stateAtTurn1 = parseGameState . unlines $ [
    "179",
    "0 690",
    "** ** 3a 3a 3i 3a 3a 3a 3a 3a 2a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "** 3a 3a 3a 0a 3a 3a 3a 3a 0a 0a 0a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 0f 3a 3a 3a 3a 3a 0a 3a 0a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 2g 3a 3a 3a 3a 3a 0a 0a 2a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3h 3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "5c 1d 6e 3a 3a 3a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 2a 3a 3a 3a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a 0a 0a 0a 3a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 3a 0a 2a 0j 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 0a 0a 0a 0a 0a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 0a 3a 0a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 0a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
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
    "0 4 C i 0",
    "*",
    "3 2 S g 0",
    "4 0 C h 2",
    "10 4 C b 0",
    "7 5 C e 0",
    "*",
    "2 10 S c 0"]

testRegisterFirstState = TestCase (do let gameInfo  = registerNewState emptyGameInfo stateAtTurn0
                                          bareState = currentBareState gameInfo
                                          state     = currentState gameInfo
                                          f         = field state
                                          reds      = redChildren state
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

testRegisterSecondState = TestCase (do let gameInfo  = registerNewState (registerNewState emptyGameInfo stateAtTurn0) stateAtTurn1
                                           bareState = currentBareState gameInfo
                                           state     = currentState gameInfo
                                           f         = field state
                                           reds      = redChildren state
                                           blues     = blueChildren state
                                       assertEqual "bare state is registered state" stateAtTurn1 bareState
                                       assertEqual "augmented state in first turn has info age 1 for space (0, 1)" 1 (infoAge (f!(0, 1)))
                                       assertEqual "augmented state in first turn has info age 0 for space (2, 3)" 0 (infoAge (f!(2, 3)))
                                       assertEqual "augmented state in first turn has correct info for space (0, 1)" (Empty 3) (contents (f!(0, 1)))
                                       assertEqual "red child 1 in field @ 0 4" (getChild1 reds) (childIn (f!(0, 4)))
                                       assertEqual "red child 3 in field @ 3 2" (getChild3 reds) (childIn (f!(3, 2)))
                                       assertEqual "red child 4 in field @ 4 0" (getChild4 reds) (childIn (f!(4, 0)))
                                       assertEqual "blue child 1 in field @ 10 4" (getChild1 blues) (childIn (f!(10, 4)))
                                       assertEqual "blue child 2 in field @ 7 5" (getChild2 blues) (childIn (f!(7, 5)))
                                       assertEqual "blue child 4 in field @ 2 10" (getChild4 blues) (childIn (f!(2, 10)))
                                   )

tests = TestList [
           TestLabel "testRegisterFirstState" testRegisterFirstState
         , TestLabel "testRegisterSecondState" testRegisterSecondState
        ]

main = runTestTT tests
