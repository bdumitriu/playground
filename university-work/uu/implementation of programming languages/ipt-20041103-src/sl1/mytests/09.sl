--
-- This test proves that pattern matching correctly fails when there's no match.
--

let multiply :: (Int, (Int, Int)) -> Int -> (Int, (Int, Int)) = \(x, (y, z)) n -> (x*n, (y*n, z*n))
in multiply (4, (5, 6, 7)) 2
ni
