import StateMonadPlus

-- Test with
--  *> run test[1-5]

-- Tests functions annotate and diagnostics
test1 :: StateMonadPlus Int String
test1 = do annotate "thing" (return 2)
           put 3
           diagnostics

-- Test diagnostics some more
test2 :: StateMonadPlus Int String
test2 = do return 3 >> return 4
           return 1
           put 5
           return 5
           diagnostics

-- Test failure (due to pattern match failure)
test3 :: StateMonadPlus Int String
test3 = do (h:t) <- return []
           return [1]
           diagnostics

-- Test saveState and loadState
test4 :: StateMonadPlus Int (Int,Int,Int,Int,Int)
test4 = do i1 <- get ; saveState
           modify (*2)
           i2 <- get ; saveState
           modify (*2)
           i3 <- get ; loadState
           i4 <- get ; loadState
           i5 <- get  
           return (i1, i2, i3, i4, i5)

-- Test failure due to loading from an empty stack
test5 :: StateMonadPlus Int (Int,Int,Int,Int,Int)
test5 = do i1 <- get ; saveState
           modify (*2)
           i2 <- get ; loadState
           modify (*2)
           i3 <- get ; loadState
           i4 <- get ; loadState
           i5 <- get  
           return (i1, i2, i3, i4, i5)

-- Auxiliary function to start with the state 1
run m = runStateMonadPlus m 1
