--
-- This test proves that pattern matching correctly fails when there's no match.
-- (different case than the one in 09.sl)
--

let multiply :: (Int, (Int, Int)) -> Int -> (Int, (Int, Int)) = \(x, (y, z)) n -> (x*n, (y*n, z*n))
in multiply (4, (5)) 2
ni
