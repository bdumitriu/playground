module Stack where

type Stack a = [a]

push :: a -> Stack a -> Stack a
push = (:)

pop :: Stack a -> Maybe (a, Stack a)
pop []    = Nothing
pop (h:t) = Just (h,t)

empty :: Stack a -> Bool
empty = null

emptyStack :: Stack a
emptyStack = []
