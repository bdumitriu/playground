module GameStateTests where

import Test.HUnit

import GameState

emptyFieldWithLineNumbers = [take 31 $ repeat (buildFieldSpace (Empty i)) | i <- [0..30]]

testEmptyFieldWithLineNumbers = TestCase (do let field = buildField emptyFieldWithLineNumbers
                                             assertEqual "field has size 31x31" ((0, 0), (30, 30)) (bounds field)
                                             assertEqual "first line is Empty 0" (Empty 0) (contents (field!(0, 0)))
                                             assertEqual "random line is Empty <line-number>" (Empty 4) (contents (field!(4, 5)))
                                             assertEqual "last line Empty 30" (Empty 30) (contents (field!(30, 30)))
                                         )

emptyFieldWithColumnNumbers = take 31 $ repeat [buildFieldSpace (Empty i) | i <- [0..30]]

testEmptyFieldWithColumnNumbers = TestCase (do let field = buildField emptyFieldWithColumnNumbers
                                               assertEqual "field has size 31x31" ((0, 0), (30, 30)) (bounds field)
                                               assertEqual "first column is Empty 0" (Empty 0) (contents (field!(30, 0)))
                                               assertEqual "random column is is Empty <column-number>" (Empty 5) (contents (field!(4, 5)))
                                               assertEqual "last column is Empty 30" (Empty 30) (contents (field!(0, 30)))
                                           )

emptyFieldWithOneTree = [[if i == 4 && j == 5 then buildFieldSpace (Tree (i + j)) else buildFieldSpace (Empty (i + j)) | j <- [0..30]] | i <- [0..30]]

testEmptyFieldWithOneTree = TestCase (do let field = buildField emptyFieldWithOneTree
                                         assertEqual "random cell is empty" (Empty 29) (contents (field!(19, 10)))
                                         assertEqual "we have a tree at (4, 5)" (Tree 9) (contents (field!(4, 5)))
                                     )

tests = TestList [
           TestLabel "testEmptyFieldWithLineNumbers" testEmptyFieldWithLineNumbers
         , TestLabel "testEmptyFieldWithColumnNumbers" testEmptyFieldWithColumnNumbers
         , TestLabel "testEmptyFieldWithOneTree" testEmptyFieldWithOneTree
        ]

main = runTestTT tests
