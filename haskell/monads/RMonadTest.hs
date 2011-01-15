{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module RMonadTest (
  step
, lift1
, lift2
, (==*)
, inc
, ifR
, factorial
, run
, (|||)
) where

import Monads

-- data R resource a = R (resource -> (resource, Either a (R resource a)))
type RInteger = R (Integer, [String])

step :: Show a => String -> a -> RInteger a
step msg v = c
    where c = R (\(r, msgs) -> if r > 0 then ((r - 1, (msg ++ ": " ++ show v):msgs), Left v)
                                        else ((r, msgs), Right c))

lift1 :: Show b => String -> (a -> b) -> (RInteger a -> RInteger b)
lift1 msg f = \ra1 -> do a1 <- ra1
                         step msg (f a1)

lift2 :: Show c => String -> (a -> b -> c) -> (RInteger a -> RInteger b -> RInteger c)
lift2 msg f = \ra1 ra2 -> do a1 <- ra1
                             a2 <- ra2
                             step msg (f a1 a2)

(==*) :: Ord a => RInteger a -> RInteger a -> RInteger Bool
(==*) = lift2 "==" (==)

instance Num a => Num (RInteger a) where
  (+)         = lift2 "+" (+)
  (-)         = lift2 "-" (-)
  negate      = lift1 "neg" negate
  (*)         = lift2 "*" (*)
  abs         = lift1 "abs" abs
  signum      = lift1 "sig" signum
  fromInteger = return . fromInteger

inc :: RInteger Integer -> RInteger Integer
inc = (+ 1)

ifR :: RInteger Bool -> RInteger a -> RInteger a -> RInteger a
ifR raIf raThen raElse = do cond <- raIf
                            if cond then raThen else raElse

factorial :: RInteger Integer -> RInteger Integer
factorial x =  ifR (x ==* 0) 1 (x * factorial (x - 1))

-- Runs a program in the embedded language given a maximum number of computation
-- steps.
run :: Integer -> RInteger a -> Maybe (a, [String])
run r (R c) = case c (r, []) of
                ((_, log), Left v) -> Just (v, log)
                _                  -> Nothing

-- Runs two programs in the embedded language in parallel (one step each) until
-- one finishes execution.
(|||) :: RInteger a -> RInteger a -> RInteger a
c1 ||| c2 = runParallel c1 c2 ("[c1] ", "[c2] ")

runParallel :: RInteger a -> RInteger a -> (String, String) -> RInteger a
runParallel c1 c2 names@(cName1, cName2) = oneStep c1 names (\c1' -> runParallel c2 c1' (cName2, cName1))
    where oneStep :: RInteger a -> (String, String) -> (RInteger a -> RInteger a) -> RInteger a
          oneStep (R c1) (cName, _) f = R (\(r, msgs) -> case c1 (1, msgs) of
                                                           ((r', (msg':msgs')), Left v) -> ((r - 1 + r', (cName ++ msg'):msgs'), Left v)
                                                           ((r', (msg':msgs')), Right c1') ->
                                                               let R next = f c1'
                                                               in next (r - 1, (cName ++ msg'):msgs'))
