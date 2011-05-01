
module StartAntTest2 (main) where

import Prelude hiding (Left, Right)
import Library
import StartAnt (mk_ant)

main :: IO ()
main = putStr $ compile ant

ant :: Program
ant = mk_ant [spin] (goto spin)

spin :: LabelledFragment
spin = "default4"
  .>:. cmdTurn Right
  .>*. jump spin

