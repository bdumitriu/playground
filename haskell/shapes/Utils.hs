module Utils (
  sigFig
) where

{-
        Solution to part 1.

        sigFig works by shifting the decimal point either to the left or to
        the right, so that all significant figures end up in the integer part
        of the number, then this number is rounded, then the decimal point
        is shifted back to its original position. Shifting is done by division
        and multiplication with the appropiate power of 10 (which is computed
        by subtracting the number of figures in the integer part of the number
        from the number of desired significant figures).

        Negative numbers are handled as well by proper use of abs (to make the
        sign go away during computation) and signum (to put it back in the end).
-}
sigFig :: RealFrac a => a -> Int -> a
sigFig x n = let
               e = 10^^(n - (nrFigures . fst . properFraction . abs $ x))
             in
               signum x * (realToFrac (round (abs x * e)) / e)

{-
        Computes the number of figures in a number in the usual way: we divide
        the number by 10 until the result becomes 0. Then we take the length
        of the list of results.
-}
nrFigures :: Integral a => a -> Int
nrFigures = length . takeWhile (>0) . iterate (`div` 10)
