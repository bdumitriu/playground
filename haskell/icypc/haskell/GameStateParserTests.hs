module GameStateParserTests where

import Test.HUnit

import GameStateParser

sampleInitialState = unlines [
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

sampleState1 = unlines $ [
    "179",
    "0 690",
    "3a 3a 3a 3a 3i 3a 3a 3a 3a 3a 2a 3a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
    "3a 3a 3a 3a 0a 3a 3a 3a 3a 0a 0a 0a ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** **",
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
    "2 3 S f 4",
    "3 2 S g 0",
    "4 0 C h 2",
    "10 4 C b 0",
    "7 5 C e 0",
    "*",
    "2 10 S c 0"]

testInitialState = TestCase (do let gameState = parseGameState sampleInitialState
                                    Just redChild3 = getChild3 . redChildren $ gameState
                                assertEqual "initial state turn is 0" 0 (turnNumber gameState)
                                assertEqual "initial state scores are 0" (0, 0) (scoreRed gameState, scoreBlue gameState)
                                assertEqual "visible field is ok" (Empty 3) (contents ((field gameState)!(7, 7)))
                                assertEqual "invisible field is ok" Unknown (contents ((field gameState)!(7, 8)))
                                assertEqual "3rd red child is in pos (2, 1)" (2, 1) (position redChild3)
                                assertEqual "3rd red child holds NotMuch" NotMuch (payload redChild3)
                                assertEqual "2nd blue child is invisible" Nothing (getChild2 . blueChildren $ gameState)
                            )

testSampleState1 = TestCase (do let gameState = parseGameState sampleState1
                                    f         = field gameState
                                    reds      = redChildren gameState
                                    Just r1   = getChild1 reds
                                    Just r2   = getChild2 reds
                                    Just r3   = getChild3 reds
                                    Just r4   = getChild4 reds
                                    blues     = blueChildren gameState
                                    Just b1   = getChild1 blues
                                    Just b2   = getChild2 blues
                                    Just b4   = getChild4 blues

                                assertEqual "state turn is 179" 179 (turnNumber gameState)
                                assertEqual "scores is 690 to 0" (0, 690) (scoreRed gameState, scoreBlue gameState)

                                assertEqual "field with 'c'" (S_SnowBall 5) (contents ((field gameState)!(5, 0)))
                                assertEqual "field with 'd'" (M_SnowBall 1) (contents ((field gameState)!(5, 1)))
                                assertEqual "field with 'e'" (MS_SnowBall 6) (contents ((field gameState)!(5, 2)))
                                assertEqual "field with 'f'" (L_SnowBall 0) (contents ((field gameState)!(2, 3)))
                                assertEqual "field with 'g'" (LM_SnowBall 2) (contents ((field gameState)!(3, 3)))
                                assertEqual "field with 'h'" (LS_SnowBall 3) (contents ((field gameState)!(4, 3)))
                                assertEqual "field with 'i'" (RedSnowman 3) (contents ((field gameState)!(0, 4)))
                                assertEqual "field with 'j'" (BlueSnowman 0) (contents ((field gameState)!(8, 7)))

                                assertEqual "1st red child is in pos (0, 4)" (0, 4) (position r1)
                                assertEqual "2nd red child is in pos (2, 3)" (2, 3) (position r2)
                                assertEqual "3rd red child is in pos (3, 2)" (3, 2) (position r3)
                                assertEqual "4th red child is in pos (4, 0)" (4, 0) (position r4)

                                assertEqual "1st blue child is in pos (10, 4)" (10, 4) (position b1)
                                assertEqual "2nd blue child is in pos (7, 5)" (7, 5) (position b2)
                                assertEqual "3rd blue child is invisible" Nothing (getChild3 blues)
                                assertEqual "4th blue child is in pos (2, 10)" (2, 10) (position b4)

                                assertEqual "1st red child is crouching" False (isStanding r1)
                                assertEqual "2nd red child is standing" True (isStanding r2)
                                assertEqual "3rd red child is standing" True (isStanding r3)
                                assertEqual "4th red child is crouching" False (isStanding r4)

                                assertEqual "1st blue child is crouching" False (isStanding b1)
                                assertEqual "2nd blue child is crouching" False (isStanding b2)
                                assertEqual "4th blue child is standing" True (isStanding b4)

                                assertEqual "child with 'b'" (Snow 1) (payload b1)
                                assertEqual "child with 'c'" (Snow 2) (payload b4)
                                assertEqual "child with 'e'" (SSnowBall 1) (payload b2)
                                assertEqual "child with 'f'" (SSnowBall 2) (payload r2)
                                assertEqual "child with 'g'" (SSnowBall 3) (payload r3)
                                assertEqual "child with 'h'" (MSnowBall) (payload r4)
                                assertEqual "child with 'i'" (LSnowBall) (payload r1)

                                assertEqual "1st red child is not dazed" 0 (dazedAnother r1)
                                assertEqual "2nd red child is dazed for 4" 4 (dazedAnother r2)
                                assertEqual "3rd red child is not dazed" 0 (dazedAnother r3)
                                assertEqual "4th red child is dazed for 2" 2 (dazedAnother r4)
                            )

tests = TestList [
           TestLabel "testInitialState" testInitialState
         , TestLabel "testSampleState1" testSampleState1
        ]

main = runTestTT tests
