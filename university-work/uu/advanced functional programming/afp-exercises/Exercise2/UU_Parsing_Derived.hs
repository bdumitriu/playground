module UU_Parsing_Derived 
where
import UU_Parsing_Core
-- import IOExts
-- =======================================================================================
-- ===== Derived Combinators =============================================================
-- =======================================================================================
infixl 4 <$>, <$, <*, *>, <**>, <??>
infixl 2 `opt`
opt     :: (Show (SymbolR s), Symbol s) => Parser s a -> a          -> Parser s a
(<$>)   :: Symbol s => (b -> a)   -> Parser s b        -> Parser s a
(<$)    :: Symbol s => b          -> Parser s c        -> Parser s b
(<*)    :: Symbol s => Parser s b -> Parser s c        -> Parser s b
(*>)    :: Symbol s => Parser s b -> Parser s c        -> Parser s c
(<**>)  :: Symbol s => Parser s b -> Parser s (b -> c) -> Parser s c
(<$$>)  :: Symbol s => (b -> c -> d) -> Parser s c -> Parser s (b -> d)
(<??>)  :: (Symbol a, Show (SymbolR a)) 
           => Parser a b -> Parser a (b -> b) -> Parser a b
pPacked :: Symbol s => Parser s b -> Parser s c -> Parser s d -> Parser s d

pFoldr      :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a b -> Parser a c
pFoldr_ng   :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a b -> Parser a c
pFoldr_gr   :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a b -> Parser a c

pFoldr1     :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a b -> Parser a c
pFoldr1_ng  :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a b -> Parser a c
pFoldr1_gr  :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a b -> Parser a c

pFoldrSep    :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a d -> Parser a b -> Parser a c
pFoldrSep_ng :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a d -> Parser a b -> Parser a c
pFoldrSep_gr :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a d -> Parser a b -> Parser a c

pFoldr1Sep    :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a d -> Parser a b -> Parser a c
pFoldr1Sep_ng :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a d -> Parser a b -> Parser a c
pFoldr1Sep_gr :: (Show (SymbolR a), Symbol a) => (b -> c -> c,c) -> Parser a d -> Parser a b -> Parser a c

pList    :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a [b]
pList_gr :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a [b]
pList_ng :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a [b]

pList1    :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a [b]
pList1_gr :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a [b]
pList1_ng :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a [b]

pListSep    :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a c -> Parser a [c]
pListSep_ng :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a c -> Parser a [c]
pListSep_gr :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a c -> Parser a [c]

pList1Sep    :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a c -> Parser a [c]
pList1Sep_ng :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a c -> Parser a [c]
pList1Sep_gr :: (Show (SymbolR a), Symbol a) => Parser a b -> Parser a c -> Parser a [c]

pChainl    :: (Show (SymbolR a), Symbol a) => Parser a (b -> b -> b) -> Parser a b -> Parser a b
pChainr    :: (Show (SymbolR a), Symbol a) => Parser a (b -> b -> b) -> Parser a b -> Parser a b
pChainl_ng :: (Show (SymbolR a), Symbol a) => Parser a (b -> b -> b) -> Parser a b -> Parser a b
pChainr_ng :: (Show (SymbolR a), Symbol a) => Parser a (b -> b -> b) -> Parser a b -> Parser a b
pChainl_gr :: (Show (SymbolR a), Symbol a) => Parser a (b -> b -> b) -> Parser a b -> Parser a b
pChainr_gr :: (Show (SymbolR a), Symbol a) => Parser a (b -> b -> b) -> Parser a b -> Parser a b

pAny :: Symbol a => (b -> Parser a c) -> [b] -> Parser a c
pAnySym :: Symbol a => [a] -> Parser a a

p `opt` v       = mnz p (p <<|> pSucceed v)   -- note that opt is greedy, if you do not want this
                                              -- use "... <|> pSucceed v"" instead
                                              -- p should not recognise the empty string
-- =======================================================================================
-- ===== Special sequential compositions =========================================
-- =======================================================================================
f <$> p         = pSucceed f     <*> p

f <$  p         = const f        <$> p
p <*  q         = (\ x _ -> x)   <$> p <*> q
p *>  q         = (\ _ x -> x)   <$> p <*> q
p <**> q        = (\ x f -> f x) <$> p <*> q
f <$$> p         = pSucceed (flip f) <*> p
p <??> q        =  p <**> (q `opt` id)
pPacked l r x   =   l *>  x <*   r
-- =======================================================================================
-- ===== Iterating parsers ===============================================================
-- =======================================================================================
pFoldr_gr      alg@(op,e)     p = mnz p pfm 
                                  where pfm = (op <$> p <*> pfm) `opt` e
pFoldr_ng      alg@(op,e)     p = mnz p pfm 
                                  where pfm = (op <$> p <*> pfm)  <|> pSucceed e
pFoldr         alg            p = pFoldr_gr alg p

pFoldr1_gr     alg@(op,e)     p = op <$> p <*> pFoldr_gr  alg p
pFoldr1_ng     alg@(op,e)     p = op <$> p <*> pFoldr_ng  alg p 
pFoldr1        alg            p = pFoldr1_gr alg  p

pFoldrSep_gr   alg@(op,e) sep p = mnz p ((op <$> p <*> pFoldr_gr alg (sep *> p)) `opt` e         )
pFoldrSep_ng   alg@(op,e) sep p = mnz p ((op <$> p <*> pFoldr_ng alg (sep *> p))  <|>  pSucceed e)
pFoldrSep      alg        sep p = pFoldrSep_gr alg sep p

pFoldr1Sep_gr  alg@(op,e) sep p = if acceptsepsilon sep then mnz p pfm else pfm
                                  where pfm = op <$> p <*> pFoldr_gr alg (sep *> p)
pFoldr1Sep_ng  alg@(op,e) sep p = if acceptsepsilon sep then mnz p pfm else pfm
                                  where pfm = op <$> p <*> pFoldr_ng alg (sep *> p)
pFoldr1Sep     alg        sep p = pFoldr1Sep_gr alg sep p

list_alg = ((:), [])

pList_gr        p = pFoldr_gr     list_alg   p
pList_ng        p = pFoldr_ng     list_alg   p
pList           p = pList_gr p

pList1_gr       p = pFoldr1_gr    list_alg   p
pList1_ng       p = pFoldr1_ng    list_alg   p
pList1          p = pFoldr1_gr    list_alg   p

pListSep_gr   s p = pFoldrSep_gr  list_alg s p
pListSep_ng   s p = pFoldrSep_ng  list_alg s p
pListSep      s p = pFoldrSep_gr  list_alg s p

pList1Sep_gr  s p = pFoldr1Sep_gr list_alg s p
pList1Sep_ng  s p = pFoldr1Sep_ng list_alg s p
pList1Sep     s p = pFoldr1Sep_ng list_alg s p

pChainr_gr op x    =  if acceptsepsilon op then mnz x r else r
                   where r = x <??> (flip <$> op <*> r)
pChainr_ng op x =  if acceptsepsilon op then mnz x r else r
                   where r = x <**> ((flip <$> op <*> r)  <|> pSucceed id)
pChainr    op x = pChainr_gr op x

pChainl_gr op x    =  if acceptsepsilon op then mnz x r else r
                      where
                       r      = (f <$> x <*> pList_gr (flip <$> op <*> x) )
                       f x [] = x
                       f x (func:rest) = f (func x) rest

pChainl_ng op x    =  if acceptsepsilon op then mnz x r else r
                   where
                    r      = (f <$> x <*> pList_ng (flip <$> op <*> x) )
                    f x [] = x
                    f x (func:rest) = f (func x) rest
pChainl    op x    = pChainl_ng op x

pAny  f l = if null l then usererror "pAny: argument may not be empty list" else foldr1 (<|>) (map f l)
pAnySym l = pAny pSym l -- used to be called pAnySym

pSeq [] = pSucceed []
pSeq (a:as) = (:) <$> pSym a <*> pSeq as

pLocate list = (pAny pSeq list)

-- =======================================================================================
-- ===== Warnings========= ===============================================================
-- =======================================================================================
mnz (Parser Zero _ (Descr _ _ _ l)) _
   =    usererror ("You are calling a list based derived combinator with a parser that accepts the empty string.\n"
                    ++
                   "We cannot handle the resulting left recursive formulation (and it is ambiguous too).\n"++
                   if null l then "There are also no non-empty alternatives for this non-terminal"
                   else "The other alternatives of this parser may start with:\n"++
                        concat (map ((++"\n").show) (map fst l))
                  )
mnz (Parser Zero _ _) _ 
   =    usererror ("You are calling a list based derived combinator with a parser that accepts the empty string.\n"
                   ++
                   "We cannot handle the resulting left recursive formulation (and it is ambiguous too).\n"
                  )
mnz _ v = {- trace "mnz succeeds"-} v

acceptsepsilon (Parser l _ _) = l == Zero


