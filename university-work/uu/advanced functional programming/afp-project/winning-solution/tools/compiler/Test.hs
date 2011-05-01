module Test where 

import Prelude hiding (Left,Right)
import Library

main :: IO ()
main = putStr $ compile program

program = makeProgram start $
    [start] ++ [move] ++ map mark [0..5]


start = "start" .>:. jump move

move = "move" .:.
       (forever ((rep 5 $ cmdMove (goto (mark 4))) .*. cmdTurn Left 
                  .*. cmdSense Here Food (goto (mark 0))))

mark :: Int -> LabelledFragment
mark n = ("mark"++show n) .:. mark' n

mark' :: Int -> Fragment
mark' 5 = cmdMark 5 .*. goto start
mark' n = cmdMark n .*. goto (mark (n+1))

