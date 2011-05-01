module DoNothing where 

import Prelude hiding (Left,Right)
import Library

main :: IO ()
main = putStr $ compile ant

ant :: Program
ant = makeProgram start [start]

start :: LabelledFragment
start = "start" .:. forever cmdWasteTime

