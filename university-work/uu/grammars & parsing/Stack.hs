module Stack
(
	Stack,
	emptyStack,
	isEmptyStack,
	push,
	pushList,
	pop,
	popList,
	top,
	split,
	mystack
)

where

data Stack x = MkS [x]
	     deriving (Eq, Show)

emptyStack :: Stack x
emptyStack = MkS []

isEmptyStack :: Stack x -> Bool
isEmptyStack (MkS xs) = null xs

push :: x -> Stack x -> Stack x
push x (MkS xs) = MkS (x:xs)

pushList :: [x] -> Stack x -> Stack x
pushList xs (MkS ys) = MkS (xs ++ ys)

pop :: Stack x -> Stack x
pop (MkS xs) = if isEmptyStack (MkS xs)
	       then error "pop on empty stack"
	       else MkS (tail xs)

popIf :: Eq x => x -> Stack x -> Stack x
popIf x stack = if top stack == x
		then pop stack
		else error "argument and top of stack do not match"

popList :: Eq x => [x] -> Stack x -> Stack x
popList xs stack = foldr popIf stack (reverse xs)

top :: Stack x -> x
top (MkS xs) = if isEmptyStack (MkS xs)
	       then error "top of empty stack"
	       else head xs

split :: Int -> Stack x -> ([x], Stack x)
split 0 stack = ([], stack)
split n (MkS []) = error "attempt to split the empty stack"
split (n+1) (MkS (x:xs)) = (x:ys, stack')
  where
  (ys, stack') = split n (MkS xs)

mystack = MkS [1, 2, 3, 4, 5, 6, 7]
