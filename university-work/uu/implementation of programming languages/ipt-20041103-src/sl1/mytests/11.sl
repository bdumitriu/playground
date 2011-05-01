--
-- Finally, a different kind of pattern matching (here, y = (7, 8)), also
-- working. Also notice the use of the !! operator.
--

let selection :: (Int, (Int, Int)) -> Int -> Int = \(x, y) n -> y !! n
in selection (4, (7, 8)) 0
ni
