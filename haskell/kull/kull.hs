module Main where

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

-- Definition of the Term data type.
-- (solution to problem 1)
data Term = TBool Bool                  -- boolean constant
          | TInt Int                    -- integer constant
          | And Term Term               -- term and term
          | Or Term Term                -- term or term
          | Not Term                    -- not term
          | Plus Term Term              -- term + term
          | Min Term Term               -- term - term
          | Eq Term Term                -- term == term
          | Lt Term Term                -- term < term
          | Gt Term Term                -- term > term
          | If Term Term Term           -- if term then term else term
          | Lam (String, Type) Term     -- function definition (lambda (x:T) t)
          | Apply Term Term             -- function application (lambda (x:T) t t or f t)
          | Var String                  -- variables
          deriving (Eq, Show)

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

-- Definition of the Type data type.
-- (solution to problem 2)
data Type = TyInt                       -- the integer type
          | TyBool                      -- the boolean type
          | TyFun Type Type             -- the function type
          deriving (Eq, Show)

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

-- Evaluates a term, given an initial environment.
-- (solution to problem 3)
eval :: [(String, Term)] -> Term -> Term

-- constants
-- just return the constant
eval env t@(TBool _)           = t
eval env t@(TInt _)            = t

-- boolean operations
-- evaluate x and y recursively, then compute x && y, x || y or not x
-- can only evaluate if both arguments are booleans
eval env (And x y)             = case (eval env x, eval env y) of
                                   (TBool xx, TBool yy) -> TBool (xx && yy)
                                   _                    -> error "You can only apply \"and\" to two boolean arguments."
eval env (Or x y)              = case (eval env x, eval env y) of
                                   (TBool xx, TBool yy) -> TBool (xx || yy)
                                   _                    -> error "You can only apply \"or\" to two boolean arguments."
eval env (Not x)               = case (eval env x) of
                                   (TBool xx)           -> TBool (not xx)
                                   _                    -> error "You can only apply \"not\" to a boolean argument."

-- arithemetic operations
-- evaluate x and y recursively, then compute x + y or x - y
-- can only evaluate if both arguments are integers
eval env (Plus x y)            = case (eval env x, eval env y) of
                                   (TInt xx, TInt yy)   -> TInt (xx + yy)
                                   _                    -> error "You can only apply \"+\" to two integer arguments."
eval env (Min x y)             = case (eval env x, eval env y) of
                                   (TInt xx, TInt yy)   -> TInt (xx - yy)
                                   _                    -> error "You can only apply \"-\" to two integer arguments."

-- relational operations
-- evaluate x and y recursively, then compute x == y, x < y or x > y
-- can only evaluate if arguments are either both booleans or both integers
eval env (Eq x y)              = case (eval env x, eval env y) of
                                   (TBool xx, TBool yy) -> TBool (xx == yy)
                                   (TInt xx, TInt yy)   -> TBool (xx == yy)
                                   _                    -> error "You can only apply \"==\" to two integer or two boolean arguments."
eval env (Lt x y)              = case (eval env x, eval env y) of
                                   (TBool xx, TBool yy) -> TBool (xx < yy)
                                   (TInt xx, TInt yy)   -> TBool (xx < yy)
                                   _                    -> error "You can only apply \"<\" to two integer or two boolean arguments."
eval env (Gt x y)              = case (eval env x, eval env y) of
                                   (TBool xx, TBool yy) -> TBool (xx > yy)
                                   (TInt xx, TInt yy)   -> TBool (xx > yy)
                                   _                    -> error "You can only apply \">\" to two integer or two boolean arguments."

-- conditional expression
-- evaluate the condition and then, depending on its value, either evaluate the then term or the else term
-- can only evaluate if the condition evaluates to a boolean
eval env (If cond tthen telse) = case (eval env cond) of
                                   (TBool True)         -> eval env tthen
                                   (TBool False)        -> eval env telse
                                   _                    -> error "The condition of an if statement must evaluate to a boolean."

-- function definition
-- just as with constants, return the term as is
eval env t@(Lam _ _)           = t

-- function application
-- evaluate the expression defined by the function which is being applied, using an environment extended with a binding
-- for the function argument. The argument is bound to the (evaluated) value to which the function is being applied
-- can only evaluate if first argument of Apply is a function
eval env (Apply f t)           = case eval env f of
                                   (Lam (name, _) def)  -> eval ((name, eval env t):env) def
                                   _                    -> error "The first argument of a function application must be a function."

-- handling variables
-- simply look up the value of the variable in the environment and evaluate it
-- short version
-- eval env (Var x)               = maybe (error ("Variable " ++ x ++ " not found.")) (eval env) $ lookup x env
-- long version
eval env (Var x)               = myMaybe value
  where value            = lookup x env
        myMaybe Nothing  = error ("Variable " ++ x ++ " not found.")
        myMaybe (Just v) = eval env v
-- (the short and the long versions are equivalent)

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

-- Infers the type of a term, given an initial enviroment.
-- (solution to problem 4)
typeof :: [(String, Type)] -> Term -> Type

-- constants
-- just return the type of the constant
typeof env (TBool _)             = TyBool
typeof env (TInt _)              = TyInt

-- boolean operations
-- case 1: if arguments to "and", "or", "not" are booleans, then the result is boolean too
-- case 2: if one or both arguments to "and", "or", "not" are not booleans, then we have a type error
typeof env (And x y)             = case (typeof env x, typeof env y) of
                                     (TyBool, TyBool) -> TyBool
                                     _                -> error "Type error: non-boolean argument to \"and\"."
typeof env (Or x y)              = case (typeof env x, typeof env y) of
                                     (TyBool, TyBool) -> TyBool
                                     _                -> error "Type error: non-boolean argument to \"and\"."
typeof env (Not x)               = case (typeof env x) of
                                     TyBool           -> TyBool
                                     _                -> error "Type error: non-boolean argument to \"not\"."

-- arithemetic operations
-- case 1: if arguments to "+", "-" are integers, then result is integer too
-- case 2: if one or both arguments to "+", "-" are not integers, then we have a type error
typeof env (Plus x y)            = case (typeof env x, typeof env y) of
                                    (TyInt, TyInt)    -> TyInt
                                    _                 -> error "Type error: non-integer argument to \"+\"."
typeof env (Min x y)             = case (typeof env x, typeof env y) of
                                    (TyInt, TyInt)    -> TyInt
                                    _                 -> error "Type error: non-integer argument to \"-\"."

-- relational operations
-- case 1: if arguments to "==", "<", ">" are either both integers or both booleans, then result is boolean
-- case 2: everything which doesn't fall into case 1 leads to a type error
typeof env (Eq x y)              = case (typeof env x, typeof env y) of
                                     (TyBool, TyBool) -> TyBool
                                     (TyInt, TyInt)   -> TyBool
                                     _                -> error "Type error: wrong arguments for \"==\"."
typeof env (Lt x y)              = case (typeof env x, typeof env y) of
                                     (TyBool, TyBool) -> TyBool
                                     (TyInt, TyInt)   -> TyBool
                                     _                -> error "Type error: wrong arguments for \"<\"."
typeof env (Gt x y)              = case (typeof env x, typeof env y) of
                                     (TyBool, TyBool) -> TyBool
                                     (TyInt, TyInt)   -> TyBool
                                     _                -> error "Type error: wrong arguments for \">\"."

-- conditional expression
-- first, the condition of the condition expression has have the boolean type
-- second, the type of the then term and the type of the else term have to be identical
-- if the previous two conditions are met, then the resulting type of the entire expression is the type of the then/else branch
-- if the previous two conditions are not met, then we have a type error
typeof env (If cond tthen telse) = case (typeof env cond) of
                                     TyBool           -> let tyThen = typeof env tthen
                                                             tyElse = typeof env telse
                                                         in  if   tyThen == tyElse
                                                             then tyThen
                                                             else error "Type error: then and else branches of if statement don't have the same type."
                                     _                -> error "Type error: non-boolean condition of if statement."

-- function definition
-- the type of a function definition will be <argument type> -> <function expression type>
-- the <argument type> is already there, while the <function expression type> is the type of the expression, inferred
-- by adding the (<argument name>, <argument type>) pair to the environment
typeof env (Lam (name, tp) def)  = TyFun tp (typeof ((name, tp):env) def)

-- function application
-- first, the function argument of the Apply has to be of the function type
-- second, the a in a -> b (the function type) has to be the same as the type of the term t, to which the function is applied
-- if both conditions are met, then the resulting type of the function application is b (from a -> b above)
-- if any of the conditions are not met, then we have a type error
typeof env (Apply f t)           = case typeof env f of
                                     (TyFun tArg tRes) -> let t' = typeof env t
                                                          in  if   tArg == t'
                                                              then tRes
                                                              else error ("Type error: function expected a " ++ show tArg ++ ", but got a " ++ show t' ++ ".")
                                     _                 -> error "Type error: trying to apply non-function as function."

-- handling variables
-- simply look up the type of the variable in the environment
-- short version
-- typeof env (Var x)               = maybe (error ("Variable " ++ x ++ " not found.")) id $ lookup x env
-- long version
typeof env (Var x)               = myMaybe value
  where value            = lookup x env
        myMaybe Nothing  = error ("Variable " ++ x ++ " not found.")
        myMaybe (Just v) = v
-- (the short and the long versions are equivalent)

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

-- Takes a term and evaluates it. Prints the result of the evaluation and its type or an error message.
-- (solution to problem 5)
kull :: Term -> IO ()
kull exp = do putStrLn ("Result type:  " ++ show expType)
              putStrLn ("Result value: " ++ show expVal)
  where expType = typeof [] exp
        expVal  = eval   [] exp

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

-- The "main" function.
main :: IO ()
main = do putStrLn "Testing term evaluation..."
          runTests testSuite1
          putStrLn ""
          putStrLn "Testing type inference..."
          runTests testSuite2

-- Takes a test suite (list of (test name, test, expected value) triplets)
-- as parameter, runs the tests and prints the results.
runTests :: Eq a => [(String, a, a)] -> IO ()
runTests ts = putStr (concat testResults)
  where testResults = map test ts

-- Takes a (test name, test, expected value) triplet and compares the
-- result of the test with the expected value. It returns a suitable
-- message as a string.
test :: Eq a => (String, a, a) -> String
test (name, test, expResult) = if   (test == expResult)
                               then (name ++ " SUCCEEDED\n")
                               else (name ++ " FAILED\n")

testSuite1 =
  [ ("test  1", eval [] (And (TBool True) (Or (TBool False) (TBool False))), TBool False)
  , ("test  2", eval [] (And (TBool True) (Or (TBool True) (TBool False))), TBool True)
  , ("test  3", eval [] (Plus (Min (TInt 10) (TInt 2)) (TInt 5)), TInt 13)
  , ("test  4", eval [] (Eq (TInt 1) (Min (TInt 5) (TInt 4))), TBool True)
  , ("test  5", eval [] (Lt (TInt 1) (Min (TInt 5) (TInt 4))), TBool False)
  , ("test  6", eval [] (Gt (TInt 1) (Min (TInt 5) (TInt 4))), TBool False)
  , ("test  7", eval [] (If (And (TBool True) (TBool False)) (TInt 5) (TInt 4)), TInt 4)
  , ("test  8", eval [("x", TBool True)] (Var "x"), TBool True)
  , ("test  9", eval [("x", Plus (TInt 10) (TInt 10))] (Plus (TInt 10) (Var "x")), TInt 30)
  , ("test 10", eval [] (Apply (Lam ("x", TyInt) (Plus (Var "x") (TInt 10))) (TInt 5)), TInt 15)
  , ("test 11", eval [("f", Lam ("x", TyInt) (Plus (Var "x") (TInt 10))), ("x", TInt 20)] (Apply (Var "f") (Var "x")), TInt 30)
  , ("test 12", eval [("f", Lam ("x", TyInt) (Plus (Var "x") (TInt 10))), ("x", TInt 20)] (Apply (Var "f") (Apply (Var "f") (Var "x"))), TInt 40)
  -- f (g x), where f x = if x then 5 else 7, g y = not y
  , ("test 13", eval [("x", TBool True)] (Apply (Lam ("x", TyBool) (If (Var "x") (TInt 5) (TInt 7))) (Apply (Lam ("y", TyBool) (Not (Var "y"))) (Var "x"))), TInt 7)
  ]

testSuite2 =
  [ ("test  1", typeof [] (TBool True), TyBool)
  , ("test  2", typeof [] (And (TBool True) (TBool False)), TyBool)
  , ("test  3", typeof [] (And (TBool True) (Or (TBool False) (TBool False))), TyBool)
  , ("test  4", typeof [] (And (TBool True) (Not (TBool False))), TyBool)
  , ("test  5", typeof [] (Plus (TInt 1) (Min (TInt 3) (TInt 2))), TyInt)
  , ("test  6", typeof [] (Eq (TInt 1) (Min (TInt 5) (TInt 4))), TyBool)
  , ("test  7", typeof [] (Lt (And (TBool True) (TBool True)) (Or (TBool False) (TBool True))), TyBool)
  , ("test  8", typeof [] (If (And (TBool True) (TBool False)) (TInt 5) (TInt 4)), TyInt)
  , ("test  9", typeof [] (If (Eq (TInt 5) (TInt 6)) (TBool True) (TBool False)), TyBool)
  , ("test 10", typeof [] (Apply (Lam ("x", TyInt) (Plus (Var "x") (TInt 10))) (TInt 5)), TyInt)
  , ("test 11", typeof [] (Lam ("x", TyInt) (Plus (Var "x") (TInt 10))), TyFun TyInt TyInt)
  , ("test 12", typeof [("x", TyBool)] (And (Var "x") (Or (Var "x") (Var "x"))), TyBool)
  -- f (g x), where f x = if x then 5 else 7, g y = not y
  , ("test 13", typeof [("x", TyBool)] (Apply (Lam ("x", TyBool) (If (Var "x") (TInt 5) (TInt 7))) (Apply (Lam ("y", TyBool) (Not (Var "y"))) (Var "x"))), TyInt)
  ]
