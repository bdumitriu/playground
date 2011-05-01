module BaseFind where 

import Prelude hiding (Left,Right)
import Library

import Walk

main :: IO ()
main = putStr $ compile ant

ant :: Program
ant = makeProgram start $ [start] ++ makeDirFrags [wrBaseFind,wrOnBase]

start :: LabelledFragment
start = "start" .:. goto (wrBaseFind 0)
	

wrBaseFind :: Int -> LabelledFragment
wrBaseFind = walkrandomly "basefind" 
                          (\_ -> skip)
                          (\_ -> skip)
                          (\dir -> cmdSense Here FoeHome (goto (wrOnBase dir)))

wrOnBase :: Int -> LabelledFragment
wrOnBase = walkrandomly "onbase" 
                         (\_ -> ifthen (invertft (cmdSense Ahead FoeHome)) turnThrice)
                         (\_ -> skip)
                         (\_ -> skip)

turnThrice :: Fragment
turnThrice = rep 3 $ cmdTurn Right