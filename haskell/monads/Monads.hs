module Monads (
  SM(..)
, readSM
, updateSM
, runSM

, R(..)
) where

--
-- A simple state monad
--

data SM s a = SM (s -> (a, s))

instance Monad (SM s) where
    -- c stands for "computation"
    -- fc stands for "function producing computation"
    -- r stands for "result"
    -- s stands for "state"
    SM c1 >>= fc2 = SM (\s0 -> let (r, s1) = c1 s0
                                   SM c2 = fc2 r
                               in c2 s1)

    return k = SM (\s -> (k, s))

-- extracts the state from the monad
readSM :: SM s s
readSM = SM (\s -> (s, s))

 -- updates the state of the monad
updateSM :: (s -> s) -> SM s ()  -- alters the state
updateSM f = SM (\s -> ((), f s))

-- run a computation in the SM monad
runSM :: s -> SM s a -> (a, s)
runSM s0 (SM c) = c s0

--
-- A resource monad
--

data R resource a = R (resource -> (resource, Either a (R resource a)))

instance Eq (R resource a) where
    (R _) == (R _) = True

instance Show (R resource a) where
    show (R _) = "wasn't really expecting to see this printed..."

instance Monad (R resource) where
    -- c stands for "computation"
    -- s stands for "suspended"
    -- f before [i]c stands for "function producing [interrupted] computation"
    -- v stands for "result _v_alue"
    -- r stands for "resource"
    -- (>>=) :: m a -> (a -> m b) -> m b
    R c1 >>= fc2 = R (\r0 -> case c1 r0 of
                               (r1, Left v) -> let R c2 = fc2 v
                                                in c2 r1
                               (r1, Right fsc) -> (r1, Right (fsc >>= fc2)))

    return v = R (\r ->  (r, Left v))

