module GaneshTest where 

import StartAnt
import Prelude hiding (Left,Right)
import Library

import Paths
import Markers 
import Marks

main :: IO ()
main = putStr $ compile ant

ant :: Program
ant = mk_ant_all mainloopFrags
            (mainloop 0)
            (mainloop 1)
            (mainloop 2)
            (mainloop 3)
            (mainloop 4)
            (mainloop 5)

turns :: Int
turns = 2

mainloopFrags :: [LabelledFragment]
mainloopFrags = pathsFrags

mainloop :: Int -> Fragment
mainloop dir = goto (findFood dir)

