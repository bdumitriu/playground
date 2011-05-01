--
-- This test proves the correct functioning of (even nested) pattern matching.
--

let multiply :: (Int, (Int, Int)) -> Int -> (Int, (Int, Int)) = \(x, (y, z)) n -> (x*n, (y*n, z*n))
in multiply (4, (5, 6)) 2
ni
