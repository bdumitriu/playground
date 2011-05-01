
module StartAntTest (main) where

import Library
import StartAnt (mk_ant_all)

main :: IO ()
main = putStr $ compile ant

ant :: Program
ant = mk_ant_all [walk0, walk1, walk2, walk3, walk4, walk5]
                 (goto walk0)
                 (goto walk1)
                 (goto walk2)
                 (goto walk3)
                 (goto walk4)
                 (goto walk5)

walk0 :: LabelledFragment
walk0 = "default0"
  .>:. cmdMove (goto walk0)
  .>*. jump walk0

walk1 :: LabelledFragment
walk1 = "default1"
  .>:. cmdMove (goto walk1)
  .>*. jump walk1

walk2 :: LabelledFragment
walk2 = "default2"
  .>:. cmdMove (goto walk2)
  .>*. jump walk2

walk3 :: LabelledFragment
walk3 = "default3"
  .>:. cmdMove (goto walk3)
  .>*. jump walk3

walk4 :: LabelledFragment
walk4 = "default4"
  .>:. cmdMove (goto walk4)
  .>*. jump walk4

walk5 :: LabelledFragment
walk5 = "default5"
  .>:. cmdMove (goto walk5)
  .>*. jump walk5

