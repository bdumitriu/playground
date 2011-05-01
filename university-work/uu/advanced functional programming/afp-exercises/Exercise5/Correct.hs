{-# OPTIONS -fth #-}
-- The above should be OPTIONS_GHC, but let's keep it like
-- this to ensure backward compatibility
module Correct where

import Language.Haskell.THSyntax

remove 0 = [| curry snd |]
remove n = lamE pXs (appsE . init $ eXs)
  where (pXs, eXs) = genPE "x" (n+1)

{-
   Questions:

    * Why can't remove be defined as a normal (non-Template) Haskell function?
    - Because its definition depends on its first argument.
    
    * Remove n with n = 0 is a special case, since it removes the function
      of an application rather than some argument. Give an intuition for the
      meaning of remove 0.
    - remove 0 is supposed to be used when the function of the application is
      either not a function or it does not yield a type correct expression
      together with the arguments that follow.

    * What happens if we choose n = 5 for the example presented above (the
      function filter is supplied only three parameters)?
    - A type error occurs as expected, since the generated function (remove 5)
      expects more arguments than it is actually given.
-}

insertHole n = lamE pXs (appsE (eXs ++ [hole]))
  where (pXs, eXs) = genPE "x" n

insert n e = lamE pXs (appsE (eXs ++ [ e ]))
  where (pXs, eXs) = genPE "x" n
  
{-
    * Explain your solution.
    - This solution is basically the same as what was done in the previous 
      insertHole, but now we also receive an expression e to include.
      Note, however, that this expression must already be an ExpQ, so an 
      example call to insert would be:
        $(insert 2 [| [] |]) foldr ((:) . toUpper) "afp 2005"
      This is necessary to guarantee type safety.
-}

permute p = lamE pXs (appsE . perm p $ eXs)
  where (pXs, eXs) = genPE "x" (fromIntegral (length p))

perm a b = map (\x -> maybe (error "invalid position") id (lookup x posb)) a 
  where posb = zip [0..] b

{-
   Questions:

    * Explain the difference between permute [0,2,1] and permute [0,2,1,3].
    - The latter case requires at least 4 arguments (i.e., a function
      application with at least 3 arguments), while the former only requires
      at least 3 arguments. However, applying them to 4 or more arguments
      results in the same behaviour.

    * The value [0,1,1] is not a valid permutation, but it can be passed to
      permute. What kind of corrections can we make by passing invalid
      permutations?
    - Using invalid permutations one can, instead, remove some arguments and
      also repeat others.
-}

parens :: Int -> Int -> ExpQ
parens n1 n2 = lamE pXs (appsE (b ++ [appsE m] ++ e))
  where (pXs, eXs) = genPE "x" (fromIntegral (n2+1))
        (b, rest)  = splitAt n1 eXs
        (m, e)     = splitAt (n2-n1+1) rest

{-
   Question:

    * What can you say about the values of the two integers that can be
      supplied to parens? Are there special cases?
    - Normally, it should be imposed that n <= m. It should also be the case
      that the arguments from n to m form a valid function application, namely
      that we don't get an invalid expression by suppling more arguments to a
      function than it takes.
-}

unparens = undefined

{-
      * Following our line of reasoning, we could fix the type error by using 
        unparens 1 3. Can you give a definition for unparens?
      - No, such a definition cannot be given, since one cannot write it by 
        hand either - that function would require a pattern match that cannot
        be done.
-}

insertDecl n e = funD name [clauses]
  where 
    name    = "insert" ++ show n
    body    = normalB (insert n e)
    clauses = clause [] body []
-- Bringing other definitions into scope can be done in a similar fashion.

hole :: ExpQ
hole =  [| error "inserted argument" |]